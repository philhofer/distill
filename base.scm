
(define *prebuilts*
  `((x86_64 . ,(include "prebuilt-x86_64.scm"))
    (aarch64 . ,(include "prebuilt-aarch64.scm"))))

(define (maybe-prebuilt conf ref)
  (and-let* ((_ (eq? conf (build-config)))
             (c (assq *this-machine* *prebuilts*))
             (c (assq ref (cdr c))))
    (cdr c)))

(define (bootstrap-base!)
  (let* ((host   (build-config))
         (build! (config->builder host))
         (built  (assq *this-machine* *prebuilts*))
         (->boot (lambda (pkg)
                   (lambda (conf)
                     (let ((old (pkg conf)))
                       (unless (package-prebuilt old)
                         (error "expected package to have a prebuilt binary:" (package-label old)))
                       (update-package old prebuilt: #f)))))
         (pkgs   `((make . ,(->boot make))
                   (busybox . ,(->boot busybox-core))
                   (execline . ,(->boot execline-tools))
                   (binutils . ,(->boot (binutils-for-target host)))
                   (gcc . ,(->boot (gcc-for-target host)))))
         (remake! (lambda ()
                    (let ((lhs (map car pkgs))
                          (rhs (apply build! (map cdr pkgs))))
                      (map cons lhs rhs))))
         (stable? (lambda (alist)
                    ;; check if the given alist and built alist are equivalent
                    (let loop ((lst alist))
                      (or (null? lst)
                          (let* ((p    (car lst))
                                 (name (car p))
                                 (art  (cdr p))
                                 (rest (cdr lst)))
                            (and (string=? (artifact-hash art)
                                           (artifact-hash (cdr (assq name (cdr built)))))
                                 (loop rest))))))))
    (let ((stage-1 (remake!)))
      (if (stable? stage-1)
        (display "prebuilts already equivalent.\n")
        (begin
          (display "outputting new prebuilts.\n")
          (with-output-to-file
            (conc "prebuilt-" *this-machine* ".scm")
            (lambda ()
              (write (list 'quote stage-1)))))))))

(define (cc-for-target conf)
  (list (gcc-for-target conf)
        (binutils-for-target conf)
        make
        execline-tools
        busybox-core))

(define *musl-version* '1.2.0)
(define *musl-src*    (remote-archive
                        (conc "https://www.musl-libc.org/releases/musl-" *musl-version* ".tar.gz")
                        "-DtKaw1hxYkV_hURoMR-00bA9TPByU0RITAnt9ELLls="))

;; when building a cross-compiler,
;; we need headers for the target system
;;
;; this package is a hack that provides a 'tool'
;; that makes libc headers appear at a particular sysroot
(define musl-headers-for-target
  (memoize-eq
    (lambda (target)
      (lambda (host)
        (make-package
          label: (conc "musl-headers-" *musl-version* "-" ($arch target))
          src:   *musl-src*
          tools: (list make execline-tools busybox-core)
          inputs: '()
          build: (make-recipe
                    script: (execline*
                               (cd ,(conc "musl-" *musl-version*))
                               (make ,(conc "DESTDIR=/out/" ($sysroot target)) ,(conc "ARCH=" ($arch target)) "prefix=/usr" install-headers))))))))

(define musl
  (lambda (conf)
    ;; note: musl is compiled as -ffreestanding, so
    ;; the gcc that builds it does *not* need to know
    ;; how to find a libc.a or crt*.o, and so forth
    (make-package
      label:  (conc "musl-" *musl-version* "-" ($arch conf))
      src:    *musl-src*
      tools:  (cc-for-target conf)
      inputs: '()
      build: (make-recipe
               ;; ./configure, but not autotools
               script: (execline*
                         (cd ,(conc "musl-" *musl-version*))
                         (if ((./configure --disable-shared --enable-static
                                           --prefix=/usr ,@(splat conf CC: CFLAGS:)
                                           --target ,($arch conf))))
                         (if ((make ,@(kvargs ($make-overrides conf)))))
                         (make DESTDIR=/out install))))))

(define libssp-nonshared
  (lambda (conf)
    (make-package
      label:   (conc "libssp-nonshared-" ($arch conf))
      tools:   (cc-for-target conf)
      inputs:  '()
      src:     (interned "/src/ssp-nonshared.c" #o644 #<<EOF
                         extern void __stack_chk_fail(void);
                         void __attribute__((visibility ("hidden"))) __stack_chk_fail_local(void) { __stack_chk_fail(); }
EOF
                         )
      build: (make-recipe
               script: (execline*
                         (cd ./src)
                         (if ((,@($CC conf) ,@($CFLAGS conf) -c ssp-nonshared.c -o __stack_chk_fail_local.o)))
                         (if ((,($AR conf) -Dcr libssp_nonshared.a __stack_chk_fail_local.o)))
                         (if ((mkdir -p /out/usr/lib)))
                         (cp libssp_nonshared.a /out/usr/lib/libssp_nonshared.a))))))

;; dependencies necessary to statically link an ordinary C executable
(define libc (list musl libssp-nonshared))

(define gawk
  (let* ((version '5.0.1)
         (leaf    (remote-archive
                    (conc "https://ftp.gnu.org/gnu/gawk/gawk-" version ".tar.xz")
                    "R3Oyp6YDumBd6v06rLYd5U5vEOpnq0Ie1MjwINSmX-4=")))
    (lambda (conf)
      (make-package
        label:  (conc "gawk-" version "-" ($arch conf))
        src:    leaf
        tools:  (cc-for-target conf)
        inputs: libc
        build:  (gnu-recipe
                  (conc "gawk-" version)
                  (kwith
                    ($gnu-build conf)
                    ;; we don't want the awk symlink;
                    ;; it conflicts with busybox
                    post-install: (+= '((foreground ((rm -f /out/usr/bin/awk)))))))))))

(define (native-toolchain)
  (list libssp-nonshared
        musl
        native-gcc
        native-binutils))

(define (native-toolchain-for conf)
  (if (eq? ($arch conf) *this-machine*)
    (list musl libssp-nonshared)
    (list native-gcc native-binutils musl libssp-nonshared)))

(define libgmp
  (let* ((version '6.2.0)
         (leaf    (remote-archive
                    (conc "https://gmplib.org/download/gmp/gmp-" version ".tar.xz")
                    "YQMYgwK95PJL5gS5-l_Iw59tc1O31Kx3X2XFdWm8t6M=")))
    (lambda (conf)
      (make-package
        label:  (conc "gmp-" version "-" ($arch conf))
        src:    leaf
        tools:  (append
                  (cons m4 (cc-for-target conf))
                  (native-toolchain-for conf))
        inputs: libc
        build:  (gnu-recipe
                  (conc "gmp-" version)
                  ;; gmp's configure script is silly and
                  ;; only looks at CC_FOR_BUILD, but doesn't
                  ;; know anything about CFLAGS_FOR_BUILD, etc
                  (let ((cc-for-build (spaced
                                        (list (kref cc-env/for-build
                                                    CC_FOR_BUILD:)
                                              (kref cc-env/for-build
                                                    CFLAGS_FOR_BUILD:)))))
                    (kwith
                      ($gnu-build conf)
                      exports: (+= (list (cons 'CC_FOR_BUILD cc-for-build))))))))))

(define libmpfr
  (let* ((version '4.0.2)
         (leaf    (remote-archive
                    (conc "https://www.mpfr.org/mpfr-current/mpfr-" version ".tar.bz2")
                    "wKuAJV_JEeh560Jgqo8Iub6opUuqOKFfQATGEJ2F3ek=")))
    (lambda (conf)
      (make-package
        label:  (conc "mpfr-" version "-" ($arch conf))
        src:    leaf
        tools:  (cc-for-target conf)
        inputs: (cons libgmp libc)
        build:  (gnu-recipe (conc "mpfr-" version) ($gnu-build conf))))))

(define libmpc
  (let* ((version '1.1.0)
         (leaf    (remote-archive
                    (conc "https://ftp.gnu.org/gnu/mpc/mpc-" version ".tar.gz")
                    "2lH9nuHFlFtOyT_jc5k4x2CHCtir_YwwX9mg6xoGuTc=")))
    (lambda (conf)
      (make-package
        label:  (conc "mpc-" version "-" ($arch conf))
        src:    leaf
        tools:  (cc-for-target conf)
        inputs: (cons* libgmp libmpfr libc)
        build:  (gnu-recipe (conc "mpc-" version) ($gnu-build conf))))))

(define m4
  (let* ((version '1.4.18)
         (leaf    (remote-archive
                    (conc "https://ftp.gnu.org/gnu/m4/m4-" version ".tar.gz")
                    "_Zto8BBAes0pngDpz96kt5-VLF6oA0wVmLGqAVBdHd0=")))
    (lambda (conf)
      (make-package
        label:  (conc "m4-" version "-" ($arch conf))
        src:    leaf
        tools:  (cc-for-target conf)
        inputs: libc
        build:  (gnu-recipe
                  (conc "m4-" version)
                  (kwith
                    ($gnu-build conf)
                    ;; m4 sticks a file in /usr/lib/charset.alias
                    post-install: (+= '((if ((rm -rf /out/usr/lib)))))))))))

(define bzip2
  (let* ((version '1.0.8)
         (leaf    (remote-archive
                    (conc "https://sourceware.org/pub/bzip2/bzip2-" version ".tar.gz")
                    "pZGXjBOF4VYQnwdDp2UYObANElrjShQaRbMDj5yef1A=")))
    (lambda (conf)
      (make-package
        label:  (conc "bzip2-" version "-" ($arch conf))
        src:    leaf
        tools:  (cc-for-target conf)
        inputs: libc
        build:  (make-recipe
                  script: (execline*
                            (cd ,(conc "bzip2-" version))
                            (make PREFIX=/out/usr ;; no DESTDIR supported
                                  ,@(kvargs ($cc-env conf))
                                  ,@(kvargs ($make-overrides conf))
                                  install)))))))

(define skalibs
  (let* ((version '2.9.1.0)
         (leaf    (remote-archive
                    (conc "https://skarnet.org/software/skalibs/skalibs-" version ".tar.gz")
                    "FlrvgEOHU_UzTJDBjUrQ0HHLijHeqstC_QgCK-NE2yo=")))
    (lambda (conf)
      (make-package
        label:  (conc "skalibs-" version "-" ($arch conf))
        src:    leaf
        tools:  (cc-for-target conf)
        inputs: libc
        build:  (ska-recipe
                  (conc "skalibs-" version)
                  (kwith
                    ($ska-build conf)
                    configure-args: (+= '(--with-sysdep-devurandom=yes))))))))

(define execline-tools
  (let* ((version '2.5.3.0)
         (leaf    (remote-archive
                    (conc "https://skarnet.org/software/execline/execline-" version ".tar.gz")
                    "qoNVKJ4tBKdrzqlq10C5rqIsv3FDzeFa9umqs7EJOdM=")))
    (lambda (conf)
      (make-package
        prebuilt: (maybe-prebuilt conf 'execline)
        label:    (conc "execline-" version "-" ($arch conf))
        src:      leaf
        tools:    (cc-for-target conf)
        inputs:   (cons skalibs libc)
        build:    (ska-recipe
                    (conc "execline-" version)
                    (kwith
                      ($ska-build conf)
                      configure-args: (+= `(,(conc "--with-sysdeps=" ($sysroot conf) "/lib/skalibs/sysdeps") --enable-static-libc))))))))


(define byacc
  (let* ((version '20191125)
         (leaf    (remote-archive
                    ;; XXX this isn't a stable tarball path;
                    ;; this will fail when upstream changes
                    "https://invisible-island.net/datafiles/release/byacc.tar.gz"
                    "2r0VA-wLi8PcDpjnyON2pyyzqY7a7tdfApRBa-HfYbg=")))
    (lambda (conf)
      (make-package
        label:  (conc "byacc-" version "-" ($arch conf))
        src:    leaf
        tools:  (cc-for-target conf)
        inputs: libc
        build:  (gnu-recipe
                  (conc "byacc-" version)
                  ($gnu-build conf))))))

(define reflex
  (let* ((version '20191123)
         (leaf    (remote-archive
                    "https://invisible-island.net/datafiles/release/reflex.tar.gz"
                    "SsgYKYUlYwedhJWxTvBAO2hdfrAMJ8mNpFjuIveGpSo=")))
    (lambda (conf)
      (make-package
        label:   (conc "reflex-" version "-" ($arch conf))
        src:     leaf
        tools:   (cons byacc (cc-for-target conf))
        inputs:  libc
        build:   (gnu-recipe
                   (conc "reflex-" version)
                   (kwith
                     ($gnu-build conf)
                     ;; install the lex(1)+flex(1) symlinks
                     post-install:
                     (+=
                       (execline*
                         (if ((ln -s reflex /out/usr/bin/lex)))
                         (if ((ln -s reflex++ /out/usr/bin/lex++)))
                         (if ((ln -s reflex /out/usr/bin/flex)))
                         (if ((ln -s reflex++ /out/usr/bin/flex++)))))))))))

(define zlib
  (let* ((version '1.2.11)
         (leaf    (remote-archive
                    (conc "https://zlib.net/zlib-" version ".tar.gz")
                    "K3Q8ig9qMtClPdpflHwS8OkSMrLVItBzEu5beP_szJA=")))
    (lambda (conf)
      (make-package
        label:  (conc "zlib-" version "-" ($arch conf))
        src:    leaf
        tools:  (cc-for-target conf)
        inputs: libc
        ;; not autoconf, but this works fine
        build:  (gnu-recipe
                  (conc "zlib-" version)
                  (kwith
                    ($gnu-build conf)
                    configure-args: (:= '(--static --prefix=/usr --libdir=/lib))))))))

(define libisl
  ;; NOTE: the latest version of isl is 0.22.1, but
  ;; it is not available through any HTTPS mirror that I have found...
  ;; the gcc infrastructure mirror only includes up to 0.18
  (let* ((version '0.18)
         (leaf    (remote-archive
                    (conc "https://gcc.gnu.org/pub/gcc/infrastructure/isl-" version ".tar.bz2")
                    "bFSNjbp4fE4N5xcaqSGTnNfLPVv7QhnEb2IByFGBZUY=")))
    (lambda (conf)
      (make-package
        label:  (conc "isl-" version "-" ($arch conf))
        src:    leaf
        tools:  (cc-for-target conf)
        inputs: (cons libgmp libc)
        build:  (gnu-recipe
                  (conc "isl-" version)
                  ($gnu-build conf))))))

;; include a file as literal text at compile time
(define-syntax include-file-text
  (er-macro-transformer
    (lambda (expr rename cmp)
      (let ((arg (cadr expr)))
        (unless (string? arg)
          (syntax-error "include-file-text expects a literal string; got" expr))
        (with-input-from-file arg read-string)))))

(define make
  (let* ((version '4.3)
         (leaf    (remote-archive
                    (conc "https://ftp.gnu.org/gnu/make/make-" version ".tar.gz")
                    "HaL2VGA5mzktijZa2L_IyOv2OTKTGkf8D-AVI_wvARc=")))
    (lambda (conf)
      (make-package
        prebuilt: (maybe-prebuilt conf 'make)
        label:    (conc "make-" version "-" ($arch conf))
        src:      leaf
        tools:    (cc-for-target conf)
        inputs:   libc
        build:    (gnu-recipe
                    (conc "make-" version)
                    (kwith
                      ($gnu-build conf)
                      ;; can't call exit(3) inside a procedure registered with atexit(3);
                      ;; just exit promptly
                      pre-configure: (+= '((if ((sed "-i" -e "s/ exit (MAKE/ _exit (MAKE/g" src/output.c)))))))))))

(define *binutils-version* '2.34)
(define *gcc-version* '9.2.0)

(define *binutils-src*
  (remote-archive
    (conc "https://ftp.gnu.org/gnu/binutils/binutils-" *binutils-version* ".tar.gz")
    "laZMIwGW3GAXUFMyvWCaHwBwgnojWVXGE94gWH164A4="))

(define *gcc-src*
  (remote-archive
    (conc "https://gcc.gnu.org/pub/gcc/releases/gcc-" *gcc-version* "/gcc-" *gcc-version* ".tar.gz")
    "nzv8PqT49a1fkkQtn60mgKbxtcTl8Mazq0swhypLLRo="))

(define (gcc-target-flags conf)
  (case ($arch conf)
    ((aarch64) '(--with-arch=armv8 --with-abi=lp64))
    ((ppc64 ppc64le) '(--with-abi=elfv2 --enable-secureplt
                                        --enable-decimal-float=no
                                        --enable-targets=powerpcle-linux))
    (else '())))

(define (triplet-depends host-arch target-arch
                         all
                         cross-build
                         foreign-target)
  (let ((+cross (lambda (lst)
                  (if (eq? *this-machine* host-arch)
                    lst
                    (append cross-build lst))))
        (+foreign (lambda (lst)
                    (if (eq? host-arch target-arch)
                      lst
                      (append foreign-target lst)))))
    (+foreign
      (+cross all))))

(define binutils-for-target
  (memoize-eq
    (lambda (target)
      (lambda (host)
        (let ((host-arch     ($arch host))
              (target-arch   ($arch target))
              (target-triple ($triple target))
              (host-triple   ($triple host)))
          (make-package
            prebuilt: (and (eq? host target) (maybe-prebuilt host 'binutils))
            label:    (conc "binutils-" *binutils-version* "-" ($arch target) "-"
                            (if (eq? host-arch target-arch)
                              "native"
                              host-arch))
            src:      *binutils-src*
            tools:    (triplet-depends host-arch target-arch
                                       ;; always depends on:
                                       (cons* byacc reflex (cc-for-target host))
                                       ;; when host != build:
                                       (native-toolchain)
                                       ;; when target != host:
                                       (list (musl-headers-for-target target)))
            inputs:   (cons zlib libc)
            build:    (gnu-recipe
                        (conc "binutils-" *binutils-version*)
                        (kwith
                          ($gnu-build host)
                          exports: (+= (list cc-env/for-build))
                          ;; 2.34: even with MAKEINFO=/bin/true, binutils refuses to build without makeinfo,
                          ;; so remove 'doc' and 'po' from subdirs
                          pre-configure: (+= '((if ((sed "-i" -e "s/^SUBDIRS =.*/SUBDIRS =/" binutils/Makefile.in)))))
                          configure-args:
                          (:= `(--disable-nls --disable-shared --enable-static
                                              --disable-multilib --enable-gold=yes --with-ppl=no
                                              --disable-install-libiberty --enable-relro
                                              --disable-plugins --enable-deterministic-archives
                                              --with-pic --with-mmap --enable-ld=default
                                              --with-system-zlib --enable-64-bit-bfd
                                              --disable-install-libbfd
                                              --prefix=/usr
                                              ;; no libdir, etc. because we discard any
                                              ;; libraries and headers produced
                                              ,(conc "--program-prefix=" target-triple "-")
                                              ,(conc "--build=" build-triple)
                                              ,(conc "--target=" target-triple)
                                              ,(conc "--host=" host-triple)
                                              ,(conc "--with-sysroot=" ($sysroot target))))
                          post-install:
                          (+= (execline*
                                (if ((rm -rf /out/usr/include)))
                                (if ((rm -rf /out/include)))
                                (if ((rm -rf /out/usr/lib)))
                                (if ((rm -rf /out/lib)))))))))))))

;; this is a hack that allows us to build a cross-gcc
;; without circular dependencies: install a fake set of
;; object files that define the same symbols that musl does
(define fake-musl-for-target
  (memoize-eq
    (lambda (target)
      (lambda (host)
        (let* ((hash "GpFZDYJsPUIYcsfZe-22Qk90kJ9albSjYSf_qTfzuuA=")
               (leaf (remote-archive
                       (conc "https://b2cdn.sunfi.sh/file/pub-cdn/" hash)
                       hash kind: 'tar.zst))
               (version '0.1))
          (make-package
            label: (conc "fakemusl-" version "-" ($arch host) "-" ($arch target))
            src:   leaf
            tools: (list (binutils-for-target target)
                         make
                         execline-tools
                         busybox-core)
            inputs: '()
            build: (let* ((outdir (filepath-join "/out" ($sysroot target))))
                     (make-recipe
                       script: (execline*
                                 (cd ,(conc "fakemusl-" version))
                                 (if ((make ,@(splat target AS: AR:) all)))
                                 (make PREFIX=/usr ,(conc "DESTDIR=" outdir) install))))))))))

(define gcc-for-target
  (memoize-eq
    (lambda (target)
      (lambda (host)
        (let ((target-arch   ($arch target))
              (host-arch     ($arch host))
              (target-triple ($triple target))
              (host-triple   ($triple host))
              (patches       (patch*
                               (include-file-text "patches/gcc/pie-gcc.patch"))))
          (make-package
            prebuilt: (and (eq? host target) (maybe-prebuilt host 'gcc))
            label: (conc "gcc-" *gcc-version* "-" ($arch target) "-"
                         (if (eq? host-arch target-arch)
                           "native"
                           host-arch))
            src:   (cons *gcc-src* patches)
            ;; TODO: when build!=host!=target, need (cc-for-target target)
            ;; and the appropriate cc-env needs to be exported
            tools: (triplet-depends host-arch target-arch
                                    ;; always needs:
                                    (cons* byacc reflex gawk (cc-for-target host))
                                    ;; when build != host:
                                    (native-toolchain)
                                    ;; when host != target:
                                    (list (binutils-for-target target)
                                          (fake-musl-for-target target)
                                          (musl-headers-for-target target)))
            inputs: (cons* libgmp libmpfr libmpc libisl zlib libc)
            build:
            (let ((conf (kwith
                          host
                          ;; CROSS_DIRECTORY_STRUCTURE ensures that even a "native" gcc
                          ;; will compile as if it is a cross-compiler
                          CFLAGS: (+= '(-DCROSS_DIRECTORY_STRUCTURE)))))
              (gnu-recipe
                (conc "gcc-" *gcc-version*)
                (kwith
                  ($gnu-build conf)
                  out-of-tree: (:= #t)
                  exports: (+= (list
                                 ($make-overrides host)
                                 cc-env/for-build
                                 ;; ordinarily gcc refuses to build as PIE
                                 ;; because that breaks pre-compiled headers (?),
                                 ;; but we don't care because we disable those anyway
                                 '(gcc_cv_no_pie . no)
                                 '(gcc_cv_c_no_pie . no)
                                 '(gcc_cv_c_no_fpie . no)))
                  pre-configure: (+=
                                   (script-apply-patches patches)
                                   (execline*
                                     ;; some makefile templates don't set AR+ARFLAGS correctly;
                                     ;; just let them take the values from the environment
                                     (if ((find "." -name Makefile.in -exec sed "-i"
                                                -e "/^AR = ar/d"
                                                -e "/^ARFLAGS = cru/d"
                                                "{}" ";")))
                                     ;; don't pull in GNU tar just to install headers;
                                     ;; force the makefile to use busybox cpio instead
                                     (if ((sed "-i"
                                               -e "s/=install-headers-tar/=install-headers-cpio/g"
                                               gcc/config.build)))))
                  configure-args:
                  (:= `(--prefix=/usr --exec-prefix=/usr --disable-lto
                                      --disable-nls --disable-shared --enable-static
                                      --disable-host-shared --enable-host-static
                                      --disable-multilib --enable-default-ssp
                                      --disable-bootstrap --disable-libssp
                                      --enable-default-pie --with-cloog=no
                                      --with-ppl=no --disable-libquadmath
                                      --disable-libgomp --disable-fixed-point
                                      --enable-__cxa_atexit --disable-libstdcxx-pch
                                      --disable-libmpx --disable-libsanitizer
                                      --enable-libstdcxx-time --disable-libitm
                                      --enable-threads --enable-tls
                                      --disable-install-libiberty
                                      --enable-relro --disable-plugins
                                      --with-pic --with-mmap --disable-symvers
                                      --enable-version-specific-runtime-libs
                                      --with-system-zlib
                                      "--enable-languages=c,c++"
                                      ,(conc "--with-sysroot=" ($sysroot target))
                                      ,(conc "--build=" build-triple)
                                      ,(conc "--target=" target-triple)
                                      ,(conc "--host=" host-triple)))
                  ;; installing compilers for different targets will
                  ;; conflict unless we limit the python gdb stuff
                  ;; to just "native" (host=target) gcc builds
                  post-install: (+= (if (eq? host-arch target-arch)
                                      '()
                                      `((if ((rm -rf ,(conc "/out/usr/share/gcc-" *gcc-version* "/python"))))))))))))))))

(define native-gcc
  (lambda (build)
    ((gcc-for-target build) build)))

(define native-binutils
  (lambda (build)
    ((binutils-for-target build) build)))

(define (busybox/config config-hash extra-inputs)
  (let* ((version '1.31.1)
         (leaf    (remote-archive
                    (conc "https://busybox.net/downloads/busybox-" version ".tar.bz2")
                    "JqkfZAknGWBuXj1sPLwftVaH05I5Hb2WusYrYuO-sJk=")))
    (lambda (conf)
      (let* ((config       (remote-file
                             #f config-hash "/src/config.head" #o644))
             (patches      (patch*
                             (include-file-text "patches/busybox/busybox-bc.patch"))))
        (make-package
          label:  (conc "busybox-" version "-" ($arch conf))
          src:    (cons* config leaf patches)
          tools:  (append
                    (cons bzip2 (cc-for-target conf))
                    (native-toolchain-for conf))
          inputs: (append libc extra-inputs)
          build:  (make-recipe
                    script: (execline*
                              (cd ,(conc "busybox-" version))
                              (export KCONFIG_NOTIMESTAMP 1)
                              ,@(script-apply-patches patches)
                              (if ((mv /src/config.head .config)))
                              (if ((make V=1
                                         ,(conc "CROSS_COMPILE=" ($triple conf) "-")
                                         ,(conc "CONFIG_SYSROOT=" ($sysroot conf))
                                         ,(conc "CONFIG_EXTRA_CFLAGS=" (spaced ($CFLAGS conf)))
                                         ,@(kvargs cc-env/for-kbuild) busybox)))
                              (if ((make V=1 busybox.links)))
                              (if ((install -D -m "755" busybox /out/bin/busybox)))
                              (if ((mkdir -p /out/usr/bin /out/sbin /out/usr/sbin)))
                              (redirfd -r 0 busybox.links)
                              (forstdin -o 0 link)
                              (importas "-i" -u link link)
                              (ln -s /bin/busybox "/out/${link}"))))))))

;; busybox-core is just enough busybox to build packages;
;; it doesn't include system utilities that would require
;; linux headers
(define busybox-core
  (lambda (conf)
    ;; busybox-core is a prebuilt bootstrap package,
    ;; so we have to update the package struct to reflect that
    (let* ((config "OE8osvZRzHk6NO3aMhnF6uyZUwlpYZtOz8LF8bR2V6k=")
           (bbpkg  ((busybox/config config '()) conf))
           (pre    (maybe-prebuilt conf 'busybox)))
      (if pre
        (update-package bbpkg prebuilt: pre)
        bbpkg))))

