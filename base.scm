
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
                    script: `((cd ,(conc "musl-" *musl-version*))
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
               script: `((cd ,(conc "musl-" *musl-version*))
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
               script: `((cd ./src)
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
                  script: `((cd ,(conc "bzip2-" version))
                            (make PREFIX=/out/usr ;; no DESTDIR supported
                                  ,@(kvargs ($cc-env conf))
                                  ,@(kvargs ($make-overrides conf))
                                  install)))))))

(define skalibs
  (let* ((version '2.9.2.0)
         (leaf    (remote-archive
                    (conc "https://skarnet.org/software/skalibs/skalibs-" version ".tar.gz")
                    "s_kjqv340yaXpye_on0w-h8_PR0M6t8Agb4dPqAMWIs=")))
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
  (let* ((version '2.6.0.0)
         (leaf    (remote-archive
                    (conc "https://skarnet.org/software/execline/execline-" version ".tar.gz")
                    "KLkA2uCEV2wf2sOEbSzdE7NAiJpolLFh6HrgohLAGFo=")))
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
                      configure-args: (+= `(,(conc "--with-sysdeps=" ($sysroot conf) "/lib/skalibs/sysdeps")
                                             --enable-pedantic-posix
                                             --enable-static-libc))))))))

(define byacc
  (let* ((version '20200330)
         (leaf    (remote-archive
                    ;; XXX this isn't a stable tarball path;
                    ;; this will fail when upstream changes...
                    (conc "https://invisible-mirror.net/archives/byacc/byacc-" version ".tgz")
                    "FTAMi_kKoQy3LVJS7qBVMo-rrgG5IlzUK10JGTUaq7c=")))
    (lambda (conf)
      (make-package
        label:  (conc "byacc-" version "-" ($arch conf))
        src:    leaf
        tools:  (cc-for-target conf)
        inputs: libc
        build:  (gnu-recipe
                  (conc "byacc-" version)
                  ($gnu-build
                    (kwith conf
                           ;; --enable-btyacc enables %destructor
                           ;; and %location tracking
                           configure-flags: (+= '(--enable-btyacc)))))))))

(define reflex
  (let* ((version '20191123)
         (leaf    (remote-archive
                    (conc "https://invisible-mirror.net/archives/reflex/reflex-" version ".tgz")
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
                       `((if ((ln -s reflex /out/usr/bin/lex)))
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
(define *gcc-version* '9.3.0)

(define *binutils-src*
  (remote-archive
    (conc "https://ftp.gnu.org/gnu/binutils/binutils-" *binutils-version* ".tar.gz")
    "laZMIwGW3GAXUFMyvWCaHwBwgnojWVXGE94gWH164A4="))

(define *gcc-src*
  (remote-archive
    (conc "https://ftp.gnu.org/gnu/gcc/gcc-" *gcc-version* "/gcc-" *gcc-version* ".tar.gz")
    "Knfr2Y-XW8XSlBKweJ5xdZ50LJhnZeMmxDafNX2LEcM="))

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
                          (+= `((if ((rm -rf /out/usr/include)))
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
                       script: `((cd ,(conc "fakemusl-" version))
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
                                   `(;; some makefile templates don't set AR+ARFLAGS correctly;
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
                    script: `((cd ,(conc "busybox-" version))
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


;; mke2fs -d <dir> does not have a way
;; to bypass timestamps and override uid/gid,
;; so this hacks in a way to at least ensure
;; that those fields get zeroed...
(define mke2fs-repro-patch #<<EOF
--- a/misc/create_inode.c
+++ b/misc/create_inode.c
@@ -17,6 +17,7 @@
 #include <time.h>
 #include <sys/stat.h>
 #include <sys/types.h>
+#include <stdlib.h>
 #include <unistd.h>
 #include <limits.h> /* for PATH_MAX */
 #include <dirent.h> /* for scandir() and alphasort() */
@@ -128,6 +129,16 @@
 	inode.i_atime = st->st_atime;
 	inode.i_mtime = st->st_mtime;
 	inode.i_ctime = st->st_ctime;
+
+        if (getenv("MKE2FS_DETERMINISTIC")) {
+                inode.i_uid = 0;
+                ext2fs_set_i_uid_high(inode, 0);
+                inode.i_gid = 0;
+                ext2fs_set_i_gid_high(inode, 0);
+                inode.i_atime = 0;
+                inode.i_mtime = 0;
+                inode.i_ctime = 0;
+        }

 	retval = ext2fs_write_inode(fs, ino, &inode);
 	if (retval)
EOF
)

;; e2fsprogs is weird and uses BUILD_CC, BUILD_CFLAGS, etc.
;; in order to indicate which CC to use for building tools
(define buildcc-env
  (cc-env/build
    (lambda (kw)
      (string->keyword
        (string-append
          "BUILD_"
          (keyword->string kw))))))

(define e2fsprogs
  (let* ((version '1.45.5)
         (src     (remote-archive
                    (conc "https://kernel.org/pub/linux/kernel/people/tytso/e2fsprogs/v" version "/e2fsprogs-" version ".tar.xz")
                    "w7R6x_QX6QpTEtnNihjwlHLBBtfo-r9RrWVjt9Nc818="))
         (patches (patch* mke2fs-repro-patch)))
    (lambda (conf)
      (make-package
        label:  (conc "e2fsprogs-" version "-" ($arch conf))
        src:    (cons src patches)
        tools:  (append (cc-for-target conf)
                        (native-toolchain-for conf))
        inputs: (list linux-headers musl libssp-nonshared)
        build:  (gnu-recipe
                  (conc "e2fsprogs-" version)
                  (kwith
                    ($gnu-build conf)
                    pre-configure: (+= (script-apply-patches patches))
                    configure-args: (+=
                                      `(--enable-symlink-install
                                         --enable-libuuid
                                         --enable-libblkid
                                         --disable-uuidd
                                         --disable-fsck
                                         ,@(kvargs buildcc-env)))
                    install-flags: (:= '("MKDIR_P=install -d" DESTDIR=/out install install-libs))))))))

(define *linux-major* 5.4)
(define *linux-patch* 28)

(define *linux-source*
  (list
    (remote-archive
      (conc "https://cdn.kernel.org/pub/linux/kernel/v5.x/linux-" *linux-major* ".tar.xz")
      "SUt0rAz8S3yXkXuSN8sG6lm4sW7Bvssxg_oAKuNjqzs=")
    (remote-file
      (conc "https://cdn.kernel.org/pub/linux/kernel/v5.x/patch-" *linux-major* "." *linux-patch* ".xz")
      "17NPZlCjiaK9xOv8DnN_ctOypA5ur3cLNafcArF9gks="
      "/src/linux.patch"
      #o644)))

(define (linux-arch-name arch)
  (case arch
    ((x86_64)        'x86_64)
    ((x86)           'i386)
    ((ppc64 ppc64le) 'powerpc)
    ((aarch64)       'arm64)
    ((armv7)         'arm)
    (else (error "unrecognized arch" arch))))

(define (linux-arch-dir-name arch)
  (let ((aname (linux-arch-name arch)))
    (case aname
      ((i386 x86_64) 'x86)
      (else aname))))

(define linux-headers
  (lambda (conf)
    (make-package
      src:    *linux-source*
      label:  (conc "linux-headers-" *linux-major* "." *linux-patch* "-" ($arch conf))
      tools:  (append (list execline-tools busybox-core make xz-utils)
                      (native-toolchain))
      inputs: '()
      build:  (make-recipe
                script: `((cd ,(conc "linux-" *linux-major*))
                          (if ((pipeline ((xzcat /src/linux.patch)))
                               (patch -p1)))
                          (if ((make ,(conc 'ARCH= (linux-arch-name ($arch conf)))
                                     ,@(kvargs cc-env/for-kbuild)
                                     headers)))
                          ;; headers_install uses rsync, which is a
                          ;; silly large dependency to pull in
                          ;; at this stage...
                          (if ((cp -r usr /out)))
                          (find /out -type f ! -name "*.h" -delete))))))

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
        label: (conc "linux-" *linux-major* "." *linux-patch* "-" name)
        tools: (append (list perl xz-utils reflex byacc libelf zlib linux-headers)
                       (native-toolchain-for conf)
                       (cc-for-target conf))
        inputs: '()
        build: (let ((make-args (append
                                  (kvargs cc-env/for-kbuild)
                                  (k=v*
                                    YACC: 'yacc ;; not bison -y
                                    ARCH: (linux-arch-dir-name ($arch conf))
                                    HOST_LIBELF_LIBS: '(-lelf -lz)))))
                 (make-recipe
                   script: `((cd ,(conc "linux-" *linux-major*))
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
      ;; a dummy date(1) binary
      (define samedate
        (interned "/bin/samedate" #o755
                  (lambda ()
                    (write-exexpr
                      '((echo "Fri Apr 3 20:09:47 UTC 2020"))
                      shebang: "#!/bin/execlineb -s0"))))
      (make-package
        label:  (conc "perl-" version "-" ($arch conf))
        src:    leaf
        tools:  (cons samedate (cc-for-target conf))
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
                                  ;; force reproducible date and uname:
                                  -A "define:osname=linux"
                                  -A "define:osvers=5.4.x"
                                  -Dmyuname=distill-builder -Dcf_by=distill-builder
                                  -Dprefix=/usr -Dprivlib=/usr/share/perl5/core_perl
                                  -Darchlib=/usr/lib/perl5/core_perl -Dvendorprefix=/usr
                                  -Dvendorlib=/usr/share/perl5/vendor_perl -Dvendorarch=/usr/lib/perl5/vendor_perl
                                  -Duselargefiles -Dusethreads -Duseshrplib=false -Dd_semctl_semun
                                  -Dman1dir=/usr/share/man/man1 -Dman3dir=/usr/share/man/man3
                                  -Dinstallman1dir=/usr/share/man/man1 -Dinstallman3dir=/usr/share/man/man3
                                  -Dman1ext=1 -Dman3ext=3pm -Ud_csh -Uusedl -Dusenm
                                  -Dusemallocwrap)))
          (make-recipe
            env:   `((BUILD_ZLIB . 0)
                     (BUILD_BZIP2 . 0)
                     (BZIP2_LIB . ,(filepath-join ($sysroot conf) "/usr/lib"))
                     (BZIP2_INCLUDE . ,(filepath-join ($sysroot conf) "/usr/include")))
            script: `((cd ,(conc "perl-" version))
                      ;; force date(1) output to be stable
                      (if ((ln -sf /bin/samedate /bin/date)))
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


(define squashfs-tools
  ;; NOTE: mksquashfs before 4.4 does not honor SOURCE_DATE_EPOCH
  ;; nor does it produce reproducible images (without a lot of additional trouble)
  (let* ((version '4.4)
         (src     (remote-archive
                    (conc "https://github.com/plougher/squashfs-tools/archive/" version ".tar.gz")
                    "o-ja9XdUjDj8KcrNOfKi7jQ1z37f7dtf3YUFgqRTIuo="))
         (patch0   (remote-file
                     "https://git.alpinelinux.org/aports/plain/main/squashfs-tools/fix-compat.patch"
                     "JnWyi6aoL93ZiPQNOEelZ1BMkuwo7JCQe1PXddsrO_Y="
                     "/src/patch0.patch"
                     #o644)))
    (lambda (conf)
      (make-package
        label:  (conc "squashfs-tools-" ($arch conf))
        src:    src
        tools:  (cc-for-target conf)
        inputs: (list zstd lz4 xz-utils zlib musl libssp-nonshared)
        build:  (make-recipe
                  script: `((cd ,(conc "squashfs-tools-" version "/squashfs-tools"))
                            ;; can't set CFLAGS= in the make invocation
                            ;; because the Makefile is clever and toggles
                            ;; a bunch of additional -DXXX flags based on configuration
                            ,@(kvexport ($cc-env conf))
                            (if ((make XZ_SUPPORT=1 LZO_SUPPORT=0
                                       LZ4_SUPPORT=1 ZSTD_SUPPORT=1 XATTR_SUPPORT=0
                                       ,@(kvargs ($make-overrides conf)))))
                            (if ((mkdir -p /out/usr/bin)))
                            (cp mksquashfs unsquashfs /out/usr/bin)))))))

(define lz4
  (let* ((version '1.9.2)
         (src     (remote-archive
                    (conc "https://github.com/lz4/lz4/archive/v" version ".tar.gz")
                    "uwHhgT74Tk7ds0TQeFZTodoI1_5IZsRnVRNNHi7ywlc=")))
    (lambda (conf)
      (make-package
        label:  (conc "lz4-" ($arch conf))
        src:    src
        tools:  (cc-for-target conf)
        inputs: (list musl libssp-nonshared)
        build:  (make-recipe
                  script: `((cd ,(conc "lz4-" version))
                            (if ((make DESTDIR=/out PREFIX=/usr
                                       ,@(kvargs ($cc-env conf))
                                       ,@(kvargs ($make-overrides conf))
                                       install)))
                            ,@(strip-binaries-script ($triple conf))))))))

(define zstd
  (let* ((version '1.4.4)
         (src     (remote-archive
                    (conc "https://github.com/facebook/zstd/archive/v"
                          version
                          ".tar.gz")
                    "PKNr93GxvtI1hA4Oia-Ut7HNNGjAcxlvfSr3TYdpdX4=")))
    (lambda (conf)
      (make-package
        label:  (conc "zstd-" ($arch conf))
        src:    src
        tools:  (cc-for-target conf)
        inputs: (list musl libssp-nonshared)
        build:
        ;; just a raw Makefile
        (let ((makeflags (append
                           (kvargs ($cc-env conf))
                           (kvargs ($make-overrides conf))
                           (k=v*
                             HAVE_PTHREAD: 1
                             HAVE_ZLIB: 0
                             HAVE_LZMA: 0
                             HAVE_LZ4:  0
                             ZSTD_LEGACY_SUPPORT: 0
                             ZSTD_LIB_DEPRECATED: 0))))
          (make-recipe
            script: `((cd ,(conc "zstd-" version))
                      (if ((cd lib/)
                           (make PREFIX=/usr DESTDIR=/out
                                 ,@makeflags install-static install-includes)))
                      (if ((cd programs/)
                           (make ,@makeflags zstd)))
                      (install -D -m "755"
                               programs/zstd /out/usr/bin/zstd))))))))

(define libarchive
  (let* ((version '3.4.1)
         (src     (remote-archive
                    (conc "https://github.com/libarchive/libarchive/releases/download/v" version "/libarchive-" version ".tar.gz")
                    "dfot337ydQKCCABhpdrALARa6QFrjqzYxFSAPSiflFk=")))
    (lambda (conf)
      (make-package
        label:  (conc "libarchive-" ($arch conf))
        src:    src
        tools:  (cc-for-target conf)
        inputs: (list bzip2 zlib xz-utils lz4 libressl zstd musl libssp-nonshared)
        build:  (gnu-recipe
                  (conc "libarchive-" version)
                  (kwith
                    ($gnu-build conf)
                    configure-args: (+= '(--without-xml2 --without-acl --without-attr --without-expat))))))))

(define libressl
  (let* ((version '3.0.2)
         (leaf    (remote-archive
                    (conc "https://ftp.openbsd.org/pub/OpenBSD/LibreSSL/libressl-" version ".tar.gz")
                    "klypcg5zlwvSTOzTQZ7M-tBZgcb3tPe72gtWn6iMTR8=")))
    (lambda (conf)
      (make-package
        label:  (conc "libressl-" version "-" ($arch conf))
        src:    leaf
        tools:  (cc-for-target conf)
        inputs: (list musl libssp-nonshared)
        build:  (gnu-recipe
                  (conc "libressl-" version)
                  (kwith
                    ($gnu-build conf)
                    post-install: (+= `((if ((ln -s openssl /out/usr/bin/libressl)))))))))))

(define xz-utils
  (let* ((version '5.2.4)
         (src     (remote-archive
                    (conc "https://tukaani.org/xz/xz-" version ".tar.xz")
                    "xbmRDrGbBvg_6BxpAPsQGBrFFAgpb0FrU3Yu1zOf4k8=")))
    (lambda (conf)
      (make-package
        label:  (conc "xz-utils-" ($arch conf))
        src:    src
        tools:  (cc-for-target conf)
        inputs: (list musl libssp-nonshared)
        build:  (gnu-recipe
                  (conc "xz-" version)
                  ($gnu-build conf))))))

(define libmnl
  (let* ((version '1.0.4)
         (src     (remote-archive
                    (conc "https://netfilter.org/projects/libmnl/files/libmnl-" version ".tar.bz2")
                    "kUOLXuIdscWD_5WHBvAIuytyuy-gGm_LXen3TWodgNs=")))
    (lambda (conf)
      (make-package
        label:   (conc "libmnl-" version "-" ($arch conf))
        src:     src
        tools:   (cc-for-target conf)
        inputs:  (list linux-headers musl libssp-nonshared)
        build:   (gnu-recipe
                   (conc "libmnl-" version)
                   ($gnu-build conf))))))

(define libnftnl
  (let* ((version '1.1.5)
         (src     (remote-archive
                    (conc "https://netfilter.org/projects/libnftnl/files/libnftnl-" version ".tar.bz2")
                    "R3Dq9wvV2TNUcXSy_KDTNcR4G8fm_ypLB9xLc0OfEBc=")))
    (lambda (conf)
      (let ()
        (make-package
          label:  (conc "libnftnl-" version "-" ($arch conf))
          src:    src
          tools:  (cc-for-target conf)
          inputs: (list linux-headers libmnl musl libssp-nonshared)
          build:  (gnu-recipe
                    (conc "libnftnl-" version)
                    (kwith
                      ($gnu-build conf)
                      ;; LIBMNL_CFLAGS needs to be set
                      ;; to make the configure script happy,
                      ;; but there isn't actually a value
                      ;; we'd like to set...
                      configure-args: (+= '(LIBMNL_CFLAGS=-lmnl
                                              LIBMNL_LIBS=-lmnl))
                      make-flags: (+= '(V=1)))))))))

(define iptables
  (let* ((version '1.8.4)
         (src     (remote-archive
                    (conc "https://netfilter.org/projects/iptables/files/iptables-" version ".tar.bz2")
                    "hBWYiIU2PYebYoMF6_n_anAFXGfruGBmAXU94ge9DQo=")))
    (lambda (conf)
      (let ((conf (kwith conf CFLAGS: (+= '(-D_GNU_SOURCE)))))
        (make-package
          label:  (conc "iptables-" version "-" ($arch conf))
          src:    src
          tools:  (append (list byacc reflex) (cc-for-target conf))
          inputs: (list linux-headers libnftnl libmnl musl libssp-nonshared)
          build:  (gnu-recipe
                    (conc "iptables-" version)
                    (kwith
                      ($gnu-build conf)
                      configure-args: (+= '(libmnl_CFLAGS=-lmnl
                                              libmnl_LIBS=-lmnl
                                              libnftnl_CFLAGS=-lnftnl
                                              libnftnl_LIBS=-lnftnl)))))))))

(define iproute2
  (let* ((version '5.5.0)
         (src     (remote-archive
                    (conc "https://kernel.org/pub/linux/utils/net/iproute2/iproute2-" version ".tar.xz")
                    "zVeW6PtWecKE9Qlx9l4NrfnQcIZNAW4HocbzuLiJOpo="))
         (patch0  (remote-file
                    "https://git.alpinelinux.org/aports/plain/main/iproute2/musl-fixes.patch"
                    "K4srcIY08guTgXv7DeGR6InxsXUKFV76vmeLao7Y0Cw="
                    "/src/musl-fixes.patch"
                    #o644))
         (patch1  (remote-file
                    "https://git.alpinelinux.org/aports/plain/main/iproute2/fix-install-errors.patch"
                    "jUzhNv5c3_lyQZ6omNKQaBvZNbpHZVkyeMuG15uq1sA="
                    "/src/fix-install-errors.patch"
                    #o644)))
    (lambda (conf)
      ;; the configure script isn't autoconf and
      ;; doesn't work without pkgconfig, but luckily
      ;; all it does is generate config.mk, so just do that directly...
      (let ((config.mk (interned
                         "/src/config.mk"
                         #o644
                         (lines/s
                           (list->seq
                             (append
                               '("YACC=yacc")
                               (splat
                                 conf
                                 CC: LDFLAGS: AR:)
                               (list
                                 "TC_CONFIG_IPSET:=y"
                                 "TC_CONFIG_NO_XT:=y"
                                 "HAVE_MNL:=y"
                                 "CFLAGS += -DHAVE_ELF -DHAVE_SETNS -DHAVE_LIBMNL"
                                 "LDLIBS += -lelf -lmnl -lz"
                                 "%.o: %.c"
                                 "\t$(CC) $(CFLAGS) -c -o $@ $<")))))))
        (make-package
          label: (conc "iproute2-" version "-" ($arch conf))
          src:   (list src patch0 patch1 config.mk)
          tools: (append (list byacc reflex)
                         (cc-for-target conf))
          inputs: (list linux-headers iptables libmnl libelf zlib musl libssp-nonshared)
          build:  (make-recipe
                    script: `((cd ,(conc "iproute2-" version))
                              (if ((cp /src/config.mk config.mk)))
                              ,@(script-apply-patches (list patch0 patch1))
                              (if ((sed "-i" -e "/^SUBDIRS/s: netem::" Makefile)))
                              (if ((make ,@(k=v* CCOPTS: ($CFLAGS conf))
                                         SHARED_LIBS=n PREFIX=/usr all)))
                              (if ((make SHARED_LIBS=n DESTDIR=/out PREFIX=/usr install)))
                              (if ((rm -rf /out/usr/share/bash-completion)))
                              (if ((rm -rf /out/var)))
                              (if ((rm -rf /out/usr/share/man)))
                              ,@(strip-binaries-script ($triple conf)))))))))

(define s6
  (let* ((version '2.9.0.1)
         (src     (remote-archive
                    (conc "https://skarnet.org/software/s6/s6-" version ".tar.gz")
                    "uwnwdcxfc7i3LTjeNPcnouhShXzpMPIG0I2AbQfjL_I=")))
    (lambda (conf)
      (make-package
        label:  (conc "s6-" ($arch conf))
        src:    src
        tools:  (cc-for-target conf)
        inputs: (list skalibs execline-tools musl libssp-nonshared)
        build:  (ska-recipe
                  (conc "s6-" version)
                  (kwith
                    ($ska-build conf)
                    configure-args: (+= `(,(conc '--with-sysdeps= ($sysroot conf) "/lib/skalibs/sysdeps")
                                           --enable-static-libc))))))))
(define s6-rc
  (let* ((version '0.5.1.1)
         (src     (remote-archive
                    (conc "https://skarnet.org/software/s6-rc/s6-rc-" version ".tar.gz")
                    "KCvqdFSKEUNPkHQuCBfs86zE9JvLrNHU2ZhEzLYY5RY=")))
    (lambda (conf)
      (make-package
        label:  (conc "s6-rc-" version "-" ($arch conf))
        src:    src
        tools:  (cc-for-target conf)
        inputs: (list s6 skalibs execline-tools musl libssp-nonshared)
        build:  (ska-recipe
                  (conc "s6-rc-" version)
                  (kwith
                    ($ska-build conf)
                    configure-args: (+= `(,(conc '--with-sysdeps= ($sysroot conf) "/lib/skalibs/sysdeps")
                                           --enable-static-libc))))))))

;; hard(8) command; an alternative to busybox halt(8)/reboot(8)/poweroff(8)
(define hard
  (let* ((version '0.1)
         (hash    "aVGnVsRk_al4CfeliyuIZsyj7LBG-GphmUM-BgHad7E=")
         (src     (remote-archive
                    (conc "https://b2cdn.sunfi.sh/file/pub-cdn/" hash)
                    hash
                    kind: 'tar.zst)))
    (lambda (conf)
      (make-package
        label:  (conc "hard-" version "-" ($arch conf))
        src:    src
        tools:  (cc-for-target conf)
        inputs: (list musl libssp-nonshared)
        build:  (make-recipe
                  script: `((cd ,(conc "hard-" version))
                            (if ((make ,@(splat conf CC: CFLAGS: LDFLAGS:)
                                       DESTDIR=/out install)))
                            ,@(strip-binaries-script ($triple conf))))))))

(define busybox-full
  (busybox/config
    "kHCLlhEuZrIcR3vjYENuNyI1a0eGB1B6APiyWjvkvok="
    (list linux-headers)))

