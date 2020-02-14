
(define *major* 5.4)
(define *patch* 19)

(define *linux-source*
  (list
    (remote-archive
      (conc "https://cdn.kernel.org/pub/linux/kernel/v5.x/linux-" *major* ".tar.xz")
      "SUt0rAz8S3yXkXuSN8sG6lm4sW7Bvssxg_oAKuNjqzs=")
    (remote-file
      (conc "https://cdn.kernel.org/pub/linux/kernel/v5.x/patch-" *major* "." *patch* ".xz")

      "FU3Y09l8z9uV-SyBgt2mNco0ZeMdWl-adEvRXaWgVko="
      "/src/linux.patch"
      #o644)))

(define (arch-name arch)
  (case arch
    ((x86_64)        'x86_64)
    ((x86)           'i386)
    ((ppc64 ppc64le) 'powerpc)
    ((aarch64)       'arm64)
    ((armv7)         'arm)
    (else (error "unrecognized arch" arch))))

(define linux-headers
  (let ()
    (lambda (conf)
      (make-package
        parallel: #f
        src:    *linux-source*
        label:  (conc "linux-headers-" *major* "." *patch* "-" (conf 'arch))
        tools:  (append (list execline-tools busybox-core make xz-utils)
                        (native-toolchain))
        inputs: '()
        build:  (make-recipe
                  script: (execline*
                            (cd ,(conc "linux-" *major*))
                            (if ((pipeline ((xzcat /src/linux.patch)))
                                 (patch -p1)))
                            (if ((make ,(conc 'ARCH= (arch-name (conf 'arch)))
                                  ,@(map pair->string= (cc-env/kbuild))
                                  headers)))
                            ;; headers_install uses rsync, which is a
                            ;; silly large dependency to pull in
                            ;; at this stage...
                            (if ((cp -r usr /out)))
                            (find /out -type f ! -name "*.h" -delete)))))))

(define (installkernel* script)
  (interned "/sbin/installkernel" #o755
            (cond
              ((string? script)
               script)
              ((pair? script)
               (with-output-to-string
                 (lambda () (write-exexpr script))))
              (else (error "don't know how to write as a script:" script)))))

;; based on reading arch/*/boot/install.sh:
;;
;; $1 = version
;; $2 = image file
;; $3 = map file
;; $4 = destination (we already know this...)
;; (and sometimes there are trailing arguments)
(define *default-installkernel* #<<EOF
#!/bin/execlineb -s3
if { mkdir -p /out/boot }
if {
  redirfd -r 0 $2
  redirfd -x 1 /out/boot/vmlinuz
  cat
}
cp $3 /out/boot/System.map

EOF
)

;; linux/config takes a name and configuration hash
;; and produces a kernel package named "linux-<version>-<name>"
;; using the given configuration file
;;
;; the kernel configuration is determined by running
;;   'make KCONFIG_ALLCONFIG=<config> allnoconfig'
;; so you can simply include a config with only the
;; explicit configuration variables you're interested in
;; and let Kbuild figure out the rest
;;
;; this build option only supports configs with CONFIG_MODULES=n
;; (i.e. self-contained kernels without loadable modules)
(define (linux/config-static name config-hash #!key (install *default-installkernel*))
  (let ((config  (remote-file #f config-hash "/src/config" #o644))
        (install (installkernel* install)))
    (lambda (conf)
      (make-package
        src:   (append (list install config) *linux-source*)
        label: (conc "linux-" *major* "." *patch* "-" name)
        tools: (append (list perl xz-utils reflex byacc)
                       (native-toolchain-for conf)
                       (cc-for-target conf))
        inputs: '()
        build: (let ((make-args (map pair->string= (cc-env/kbuild))))
                 (make-recipe
                   script: (execline*
                             (cd ,(conc "linux-" *major*))
                             (importas -u "-i" nproc nproc)
                             (if ((pipeline ((xzcat /src/linux.patch)))
                                  (patch -p1)))
                             ;; reflex: %option full is not compatible with %option yylineno
                             (if ((sed "-i" -e "/^%option/s/ full / /" scripts/kconfig/lexer.l)))
                             ;; byacc: use -H <file> instead of --defines=<file>
                             (if ((sed "-i" -e "/cmd_bison/s/--defines=/-H /" scripts/Makefile.host)))
                             ;; byacc: doesn't support %destructor
                             (if ((sed "-i" -e "/^%destructor/,/^}/d" scripts/kconfig/parser.y)))
                             (export KCONFIG_NOTIMESTAMP 1)
                             (export KBUILD_BUILD_TIMESTAMP "@0")
                             (export KBUILD_BUILD_USER distill)
                             (export KBUILD_BUILD_HOST distill)
                             (export CROSS_COMPILE ,(conc (triple conf) "-"))
                             (if ((make
                                    V=1 YACC=yacc
                                    KCONFIG_ALLCONFIG=/src/config
                                    ,@make-args
                                    allnoconfig)))
                             (if ((make -j $nproc V=1 ,@make-args)))
                             (make install))))))))

(define linux-virt-x86_64
  (linux/config-static "virt-x86_64" "FTMQoxE4ClKOWLDdcDVzWt8UuizXfMmR4duX8Z-5qlY="))

;; perl packages a statically-linked perl binary
;;
;; NOTE: cross-compiling perl using the standard 'Configure' script
;; requires ssh access to the target machine (in order to run code!),
;; which is very obviously not something we can support, so this
;; code would have to learn *a lot* about perl build configuration internals
;; in order to make cross-compilation work
(define perl
  (let* ((version '5.30.1)
         (leaf    (remote-archive
                    (conc "https://www.cpan.org/src/5.0/perl-" version ".tar.gz")
                    "EBvfwKXjX_aaet0AXxwJKJGpGy4RnUrrYjh-qLeZ8ts=")))
    (lambda (conf)
      (unless (eq? (conf 'arch) *this-machine*)
        (fatal "one does not simply cross-compile perl; please read the comment(s) in perl.scm"))
      (make-package
        label:  (conc "perl-" version "-" (conf 'arch))
        src:    leaf
        tools:  (cc-for-target conf)
        inputs: (list zlib bzip2 musl libssp-nonshared)
        build:
        (let* ((cenv            (cc-env conf))
               (menv            (make-env conf))
               (toolpre         (conc (triple conf) "-"))
               (CC              (assq 'CC cenv))
               (LD              (assq 'LD cenv))
               (CFLAGS          (assq 'CFLAGS cenv))
               (LDFLAGS         (assq 'LDFLAGS cenv))
               (NM              (assq 'NM menv))
               (RANLIB          (assq 'RANLIB menv))
               (AR              (assq 'AR menv))
               (ARFLAGS         (assq 'ARFLAGS menv))
               (-Dflags         (map pair->string=
                                     `((-Dccflags .  ,(cdr CFLAGS))
                                       (-Darflags .  ,(cdr ARFLAGS))
                                       (-Dldflags .  ,(cdr LDFLAGS))
                                       (-Dcc .       ,(cdr CC))
                                       (-Dld .       ,(cdr LD))
                                       (-Doptimize . ,(or (conf 'CFLAGS) '-Os))
                                       (-Dnm       . ,(conc (triple conf) "-nm"))
                                       (-Dranlib   . ,(cdr RANLIB))
                                       (-Dar       . ,(cdr AR))
                                       (-Dsysroot  . ,(sysroot conf)))))
               ;; yes, this is gross
               ;; but not as gross as perl's 'Configure' script
               (configure-flags `(,@-Dflags
                                   -Dprefix=/usr -Dprivlib=/usr/share/perl5/core_perl
                                   -Darchlib=/usr/lib/perl5/core_perl -Dvendorprefix=/usr
                                   -Dvendorlib=/usr/share/perl5/vendor_perl -Dvendorarch=/usr/lib/perl5/vendor_perl
                                   -Duselargefiles -Dusethreads -Duseshrplib=false -Dd_semctl_semun
                                   -Dman1dir=/usr/share/man/man1 -Dman3dir=/usr/share/man/man3
                                   -Dinstallman1dir=/usr/share/man/man1 -Dinstallman3dir=/usr/share/man/man3
                                   -Dman1ext=1 -Dman3ext=3pm -Dcf_by=sysplan -Ud_csh -Uusedl -Dusenm
                                   -Dusemallocwrap)))
          (make-recipe
            env:   `((BUILD_ZLIB . 0)
                     (BUILD_BZIP2 . 0)
                     (BZIP2_LIB . ,(filepath-join (sysroot conf) "/usr/lib"))
                     (BZIP2_INCLUDE . ,(filepath-join (sysroot conf) "/usr/include")))
            script: (execline*
                      (cd ,(conc "perl-" version))
                      (if ((./Configure -des ,@configure-flags)))
                      (if ((importas -u nproc nproc)
                           (make -j $nproc ,@(makeflags conf))))
                      (if ((make DESTDIR=/out install)))
                      (if ((rm -rf /out/usr/share/man)))
                      (find /out -name ".*" -delete))))))))

