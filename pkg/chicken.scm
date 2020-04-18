(import
  scheme
  (only (chicken string) conc)
  (only (chicken port) with-output-to-string)
  (chicken module)
  (distill plan)
  (distill execline)
  (distill package)
  (distill kvector)
  (distill memo)
  (distill base))

(export
  libchicken
  chicken-for-target
  egg
  matchable-egg
  srfi-13-egg
  srfi-14-egg
  srfi-69-egg)

;; TODO: add support for just compiling
;; libchicken for targets that do not need
;; csi(1), csc(1), etc.

(define (without syms lst)
  (if (null? lst)
    lst
    (let ((head (car lst)))
      (if (memq head syms)
        (cdr lst)
        (cons head (without syms (cdr lst)))))))

(define (chicken-arch conf)
  (let ((arch ($arch conf)))
    (case arch
      ((x86_64) 'x86-64)
      (else     arch))))

(define (%chicken-src name targets install)
  (let ((src (source-template
               "chicken" "5.2.0"
               "https://code.call-cc.org/releases/$version/$name-$version.tar.gz"
               "M4edTDnvYOsgcWLxIoMpBTE0bXpG3wS3BmfiHiJ9_uA=")))
    (lambda (host)
      (let* ((copts     (append
                          (without
                            ;; select optimization with OPTIMIZE_FOR_{SIZE,SPEED};
                            ;; don't use -fstack-protector* because the
                            ;; generated C code is all noreturn
                            '(-fstack-protector
                               -fstack-protector-strong
                               -Os -O2 -O3)
                            ($CFLAGS host))
                          ;; necessary for building chicken
                          '(-fno-strict-aliasing -fwrapv -DHAVE_CHICKEN_CONFIG_H)))
             (mflags     '(PREFIX=/usr
                            STATICBUILD=1
                            PLATFORM=linux
                            DESTDIR=/out
                            OPTIMIZE_FOR_SPEED=1
                            ;; by default, csc should simply
                            ;; produce native binaries using
                            ;; gcc --sysroot=/
                            TARGET_C_COMPILER=gcc
                            TARGET_CXX_COMPILER=g++
                            TARGET_LIBRARIAN=ar
                            ;; gcc will take the last --sysroot argument,
                            ;; so setting it to / here is fine; we'll
                            ;; override it as necessary with wrapper scripts
                            "TARGET_LINKER_OPTIONS=--sysroot=/"
                            "TARGET_C_COMPILER_OPTIONS=--sysroot=/ -fPIE -fno-strict-aliasing -fwrapv -DHAVE_CHICKEN_CONFIG_H"
                            "LINKER_EXECUTABLE_OPTIONS=-static-pie -L."))
             (kvflags    (k=v*
                           ARCH: (chicken-arch host)
                           C_COMPILER: ($CC host)
                           CXX_COMPILER: ($CXX host)
                           LIBRARIAN: ($AR host)
                           LIBRARIAN_OPTIONS: ($ARFLAGS host)
                           C_COMPILER_OPTIONS: copts
                           LINKER_OPTIONS: ($LDFLAGS host))))

        (source->package
          host
          src
          tools:  (cc-for-target host)
          inputs: (list musl libssp-nonshared)
          build:  `((if ((make ,@mflags ,@kvflags chicken-config.h)))
                    (if ((make ,@mflags ,@kvflags ,@targets)))
                    (if ((make ,@mflags ,@kvflags ,@install)))
                    (rm -rf /out/usr/share)))))))

(define chicken
  (%chicken-src "chicken" '(all) '(install)))

(define libchicken
  (%chicken-src "libchicken" '(libchicken.a) '(install-dev)))

;; chicken-wrappers are wrappers for chicken-install and csc
;; named ${triple}-chicken-install and ${triple}-csc that
;; tweak CSC_OPTIONS so that eggs and other programs can
;; be cross-compiled correctly
(define (chicken-wrappers conf)
  (let* ((real-features (case ($arch conf)
                          ((x86_64)
                           '(64bit little-endian x86-64))
                          ((aarch64)
                           '(64bit little-endian arm64 aarch64))
                          ((armv7)
                           '(32bit litte-endian arm))
                          ((ppc64)
                           '(64bit big-endian ppc64))
                          ((ppc64le)
                           '(64bit little-endian ppc64le))))
         (clear-features '(64bit 32bit big-endian little-endian x86-64 aarch64 x32 i386 arm ppc64))
         (fscript        (interned
                           (conc "/usr/lib/chicken/" ($triple conf) "-features.scm")
                           #o644
                           (with-output-to-string
                             (lambda ()
                               (write `((import (chicken platform))
                                        (unregister-feature! ,@clear-features)
                                        (register-feature! ,@real-features)))))))
         (splat*         (lambda (flag lst)
                           (let loop ((lst lst))
                             (if (null? lst)
                               lst
                               (cons flag (cons (car lst)
                                                (loop (cdr lst))))))))
         (csc-opts       `(-cc ,($CC conf)
                               ,@(splat* '-C ($CFLAGS conf))
                               -ld ,($CC conf) ;; chicken wants LD=CC
                               ,@(splat* '-L ($LDFLAGS conf))
                               ,@(splat* '-no-feature (without real-features clear-features))
                               ,@(splat* '-feature real-features)))
         (csc-script     (interned
                           (conc "/usr/bin/" ($triple conf) "-csc")
                           #o755
                           (lambda ()
                             (write-exexpr
                               `((export CSC_OPTIONS ,(spaced csc-opts))
                                 (csc "$@"))
                               shebang: "#!/bin/execlineb -s0"))))
         (ckn-install    (interned
                           (conc "/usr/bin/" ($triple conf) "-chicken-install")
                           #o755
                           (lambda ()
                             (write-exexpr
                               `((export CSC_OPTIONS ,(spaced csc-opts))
                                 (chicken-install "$@"))
                               shebang: "#!/bin/execlineb -s0")))))
    (list csc-script ckn-install)))

(define chicken-for-target
  (memoize-eq
    (lambda (target)
      (cons chicken (append (chicken-wrappers target) (cc-for-target target))))))

;; egg defines a package for the egg 'name-version'
;; that contains the static bits of the egg
(define (egg name version hash . deps)
  (let ((src (source-template
               name version
               "https://code.call-cc.org/egg-tarballs/5/$name/$name-$version.tar.gz"
               hash)))
    (lambda (conf)
      (let ((ckn-install (conc ($triple conf) '-chicken-install)))
        (source->package
          conf
          src
          tools:  (append (chicken-for-target conf) deps)
          inputs: (append (list libchicken musl libssp-nonshared) deps)
          build:  `((backtick -n "-i" repo ((chicken-install -repository)))
                    (importas "-i" -u repo repo)
                    (export TMP /tmp)
                    (export CHICKEN_INSTALL_REPOSITORY "/out/${repo}")
                    ;; -host is just tricking chicken-install
                    ;; into compiling statically
                    (,ckn-install -host -no-install-dependencies)))))))

(define matchable-egg
  (egg "matchable" "1.1" "rygA7BrZVhDdv2x9ksqaK0fgKIc-nYT9unDIHOYlQI4="))

(define srfi-14-egg
  (egg "srfi-14" "1.4" "NwafERqp27VVghFvWoyqQooQdJ7-evsJVx-KA6Q3-7I="))

(define srfi-13-egg
  (egg "srfi-13" "1.4" "ceY9c3sFKZVzpJZT4Z6uCqNgZAH81hbl0Di8OX3zAik="
       srfi-14-egg))

(define srfi-69-egg
  (egg "srfi-69" "1.2" "aJQMPPCpKVg_53zI_5YzvK6w-ZWwGaeCORJtAjM_Fyc="))
