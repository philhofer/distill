
(define *major* 5.4)
(define *patch* 28)

(define *linux-source*
  (list
    (remote-archive
      (conc "https://cdn.kernel.org/pub/linux/kernel/v5.x/linux-" *major* ".tar.xz")
      "SUt0rAz8S3yXkXuSN8sG6lm4sW7Bvssxg_oAKuNjqzs=")
    (remote-file
      (conc "https://cdn.kernel.org/pub/linux/kernel/v5.x/patch-" *major* "." *patch* ".xz")
      "17NPZlCjiaK9xOv8DnN_ctOypA5ur3cLNafcArF9gks="
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

(define (arch-dir-name arch)
  (let ((aname (arch-name arch)))
    (case aname
      ((i386 x86_64) 'x86)
      (else aname))))

(define linux-headers
  (let ()
    (lambda (conf)
      (make-package
        src:    *linux-source*
        label:  (conc "linux-headers-" *major* "." *patch* "-" ($arch conf))
        tools:  (append (list execline-tools busybox-core make xz-utils)
                        (native-toolchain))
        inputs: '()
        build:  (make-recipe
                  script: `((cd ,(conc "linux-" *major*))
                            (if ((pipeline ((xzcat /src/linux.patch)))
                                 (patch -p1)))
                            (if ((make ,(conc 'ARCH= (arch-name ($arch conf)))
                                  ,@(kvargs cc-env/for-kbuild)
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

;; yypush_buffer_state() and yypop_buffer_state()
;; are GNU extensions
(define portable-lexer-patch #<<EOF
--- a/scripts/dtc/dtc-lexer.l
+++ b/scripts/dtc/dtc-lexer.l
@@ -277,15 +277,21 @@

 %%

+#define __MAX_INCLUDES 32
+static int __n_includes = 0;
+static void *__fstack[__MAX_INCLUDES];
+
 static void push_input_file(const char *filename)
 {
 	assert(filename);
+        assert(__n_includes < __MAX_INCLUDES);

 	srcfile_push(filename);
+        __fstack[__n_includes++] = YY_CURRENT_BUFFER;

 	yyin = current_srcfile->f;

-	yypush_buffer_state(yy_create_buffer(yyin, YY_BUF_SIZE));
+	yy_switch_to_buffer(yy_create_buffer(yyin, YY_BUF_SIZE));
 }


@@ -293,8 +299,8 @@
 {
 	if (srcfile_pop() == 0)
 		return false;
-
-	yypop_buffer_state();
+        yy_delete_buffer(YY_CURRENT_BUFFER);
+        yy_switch_to_buffer(__fstack[--__n_includes]);
 	yyin = current_srcfile->f;

 	return true;
EOF
)

;; common fixes to scripts/dtc between linux and uboot;
;; the dtc yacc+lex files do not play nicely with BSD yacc+lex
(define (fix-dtc-script #!key
                        (fix-lex-options #f)  ;; scripts/kconfig/<foo>.l
                        (fix-yacc-cmdline #f)) ;; scripts/Makefile.host or scripts/Makefile.lib
  (unless (and fix-lex-options fix-yacc-cmdline)
    (error "fix-dtc-script is missing required keyword arguments"))
  `((if ((sed "-i" -e "/^%option/s/ full / /" ,fix-lex-options)))
    (if ((sed "-i" -e "3a override YACC:=$(YACC) -L"
              scripts/dtc/Makefile)))
    (if ((sed "-i"
              -e "/^extern.*yyerror(/a #define YYERROR_CALL(msg) yyerror(msg)" scripts/dtc/dtc-parser.y)))
    ;; byacc: use -H <file> instead of --defines=<file>
    (if ((sed "-i" -e "/cmd_bison/s/--defines=/-H /" ,fix-yacc-cmdline)))))

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
(define (linux/config-static name config-hash #!key
                             (install *default-installkernel*)
                             (dtb     #f))
  (let ((config  (remote-file #f config-hash "/src/config" #o644))
        (install (installkernel* install))
        (patches (patch* portable-lexer-patch)))
    (lambda (conf)
      (make-package
        src:   (append (list install config) *linux-source* patches)
        label: (conc "linux-" *major* "." *patch* "-" name)
        tools: (append (list perl xz-utils reflex byacc libelf zlib linux-headers)
                       (native-toolchain-for conf)
                       (cc-for-target conf))
        inputs: '()
        build: (let ((make-args (append
                                  (kvargs cc-env/for-kbuild)
                                  (k=v*
                                    YACC: 'yacc ;; not bison -y
                                    ARCH: (arch-dir-name ($arch conf))
                                    HOST_LIBELF_LIBS: '(-lelf -lz)))))
                 (make-recipe
                   script: `((cd ,(conc "linux-" *major*))
                             (if ((pipeline ((xzcat /src/linux.patch)))
                                  (patch -p1)))
                             ,@(script-apply-patches patches)
                             ,@(fix-dtc-script
                                 fix-lex-options: 'scripts/kconfig/lexer.l
                                 fix-yacc-cmdline: 'scripts/Makefile.host)
                             ;; libelf is built with zlib, so -lelf should imply -lz
                             (if ((find "." -type f -name "Make*"
                                        -exec sed "-i" -e
                                        "s/-lelf/-lelf -lz/g" "{}" ";")))
                             (export KCONFIG_NOTIMESTAMP 1)
                             (export KBUILD_BUILD_TIMESTAMP "@0")
                             (export KBUILD_BUILD_USER distill)
                             (export KBUILD_BUILD_HOST distill)
                             (export CROSS_COMPILE ,(conc ($triple conf) "-"))
                             (if ((make
                                    V=1 KCONFIG_ALLCONFIG=/src/config
                                    ,@make-args
                                    allnoconfig)))
                             (if ((make V=1 ,@make-args)))
                             ,@(if dtb
                                 `((if ((install -D -m "644" -t /out/boot ,dtb))))
                                 '())
                             (make V=1 ,@make-args install))))))))

(define linux-virt-x86_64
  (linux/config-static "virt-x86_64" "FTMQoxE4ClKOWLDdcDVzWt8UuizXfMmR4duX8Z-5qlY="))

;; libelf is just a subset of elfutils (just the bit we need in order to build kbuild's objtool)
(define libelf
  (let* ((version '0.178)
         (leaf    (remote-archive
                    (conc "https://sourceware.org/elfutils/ftp/" version "/elfutils-" version ".tar.bz2")
                    "ibDvVn8CMIhlIQAZGAsl7Yf13fm42qO7NJDctqLd2Hc="))
         (config  (remote-file
                    #f "ralu1MH7h3xuq7QdjYTneOmZLEoU1RygVrTAWaKl2YY=" "/src/config.h" #o644)))
    (lambda (conf)
      (make-package
        label:  (conc "libelf-" version "-" ($arch conf))
        src:    (list leaf config)
        tools:  (cc-for-target conf)
        inputs: (list zlib musl libssp-nonshared)
        build:  (let ((cflags (append ($CFLAGS conf)
                                      '(-D_GNU_SOURCE -DHAVE_CONFIG_H -I. -I.. -I../lib)))
                      (cc     ($CC conf)))
                  (make-recipe
                    ;; ALLLLRIGHTY THEN, here's what happening here...
                    ;; elfultils' configure script is utterly broken
                    ;; and also requires a bunch of libraries that
                    ;; I don't want to package, so we're just building
                    ;; libelf.a manually and ignoring everything else
                    script: `((cd ,(conc "elfutils-" version))
                              (if ((cp /src/config.h config.h)))
                              (if ((find lib/ libelf/ -type f -name "*.[ch]"
                                         -exec sed "-i"
                                         -e "/#include <libintl.h>/d"
                                         -e "/#include.*cdefs.h>/d"
                                         "{}" ";")))
                              (if ((find lib/ -type f -name "*.h"
                                         -exec sed "-i" -e "s/<error.h>/<err.h>/g" "{}" ";")))
                              (cd libelf)
                              (if ((elglob -s csrc "*.c")
                                   (if ((echo "cflags: " ,@cflags)))
                                   (if ((echo "source: " $csrc)))
                                   (,@cc ,@cflags -c $csrc)))
                              (if ((elglob -s objs "*.o")
                                   (,($AR conf) Dcr libelf.a $objs)))
                              (if ((mkdir -p /out/usr/include /out/usr/lib)))
                              (if ((cp gelf.h /out/usr/include/gelf.h)))
                              (if ((cp libelf.h /out/usr/include/libelf.h)))
                              (cp libelf.a /out/usr/lib/libelf.a))))))))

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
      ;; TODO: really, this test should be even tighter:
      ;; you can't build perl on a machine that can't run
      ;; the perl executable produced using $CC, which
      ;; means that i686/x86_64 builds and ppc{32,64}{,le}
      ;; builds may or may not work, depending on how
      ;; the kernel is configured...
      (unless (eq? ($arch conf) *this-machine*)
        (fatal "one does not simply cross-compile perl :("))
      (make-package
        label:  (conc "perl-" version "-" ($arch conf))
        src:    leaf
        tools:  (cc-for-target conf)
        inputs: (list zlib bzip2 musl libssp-nonshared)
        build:
        ;; yes, this is gross
        ;; but not as gross as perl's 'Configure' script
        (let ((configure-flags `(,@(k=v*
                                     -Dcc:       ($CC conf)
                                     -Dccflags:  ($CFLAGS conf)
                                     -Dar:       ($AR conf)
                                     -Darflags:  ($ARFLAGS conf)
                                     -Dld:       ($LD conf)
                                     -Dldflags:  ($LDFLAGS conf)
                                     -Doptimize: '-Os ;; TODO: figure out how to pull this out of $CFLAGS
                                     -Dnm:       ($NM conf)
                                     -Dranlib:   ($RANLIB conf)
                                     -Dsysroot:  ($sysroot conf))
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
                     (BZIP2_LIB . ,(filepath-join ($sysroot conf) "/usr/lib"))
                     (BZIP2_INCLUDE . ,(filepath-join ($sysroot conf) "/usr/include")))
            script: `((cd ,(conc "perl-" version))
                      (if ((./Configure -des ,@configure-flags)))
                      (if ((make ,@(kvargs ($make-overrides conf)))))
                      (if ((make DESTDIR=/out install)))
                      (if ((rm -rf /out/usr/share/man)))
                      (find /out -name ".*" -delete))))))))

;; busybox xxd doesn't recognize '-i'
;; but we can achieve a similar result
;; using busybox's hexdump
(define no-xxd-patch #<<EOF
--- a/Makefile
+++ b/Makefile
@@ -1831,7 +1831,7 @@
 	 grep -v '^$$' | \
 	 tr '\n' '\0' | \
 	 sed -e 's/\\\x0/\n/g' | \
-	 xxd -i ; echo ", 0x00" ; )
+	 hexdump -v -e '/1 "0x%X, "' ; echo "0x00" ; )
 endef

 $(version_h): include/config/uboot.release FORCE
EOF
)

;; uboot/config accepts 5 arguments:
;;  - name: a suffix added to the package name
;;    (the package will be named "uboot-<version>-<name>"
;;  - hash: the hash of the .config
;;  - env:  a list of key=value strings that populate
;;    the default environment for the bootloader
;;  - bootargs: the default kernel argument list
;;  - bootcmd: the default kernel boot command for u-boot (i.e. booti, etc.)
(define uboot/config
  (let* ((version '2020.04-rc3)
         (src     (remote-archive
                    (conc "https://ftp.denx.de/pub/u-boot/u-boot-" version ".tar.bz2")
                    "-se2Ch0_yG0gCjtkTSEUmOYrle8860Gg887w3f7I8yI=")))
    (lambda (name h env bootargs bootcmd)

      (let ((patches (patch* no-xxd-patch portable-lexer-patch))
            (envfile (interned
                       "/src/uboot-env"
                       #o644
                       (lines/s (list->seq env))))
            (dotconf (remote-file
                       #f h "/src/uboot-config" #o644)))
        (lambda (conf)
          (make-package
            label: (conc "uboot-" version "-" name)
            src:   (cons* src envfile dotconf patches)
            tools: (append
                     (list reflex byacc)
                     (cc-for-target conf)
                     (native-toolchain-for conf))
            inputs: '()
            build: (let ((make-args (append
                                      (k=v*
                                        YACC: '(yacc -d)
                                        CONFIG_BOOTARGS: bootargs
                                        CONFIG_BOOTCOMMAND: bootcmd
                                        CROSS_COMPILE: (conc ($triple conf) "-"))
                                      (kvargs cc-env/for-kbuild))))
                     ;; note that we don't really do much in terms of
                     ;; setting the usual CFLAGS=..., LDFLAGS=... here,
                     ;; because those flags likely do not apply safely
                     ;; to building a freestanding bootloader
                     (make-recipe
                       script: `((cd ,(conc "u-boot-" version))
                                 (importas -D 0 epoch SOURCE_DATE_EPOCH)
                                 (backtick SOURCE_DATE_EPOCH
                                           ((pipeline ((echo -n $epoch)))
                                            (sed -e "s/@//")))
                                 ,@(script-apply-patches patches)
                                 (if ((cp /src/uboot-config .config)))
                                 ,@(fix-dtc-script
                                     fix-lex-options: 'scripts/kconfig/zconf.l
                                     fix-yacc-cmdline: 'scripts/Makefile.lib)
                                 ;; we're using yacc -d, so the zconf.tab.c needs to #include the generated definitions
                                 (if ((sed "-i"
                                           -e "/^#include \"/a #include \"zconf.tab.h\"" scripts/kconfig/zconf.y)))
                                 (if ((make V=1 ,@make-args)))
                                 (install -D -m 644 -t /out/boot u-boot.bin))))))))))

