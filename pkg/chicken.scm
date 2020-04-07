(import
  scheme
  (only (chicken string) conc)
  (chicken module)
  (distill plan)
  (distill package)
  (distill kvector)
  (distill memo)
  (distill base))

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

(export chicken-for-target)
(define chicken-for-target
  (memoize-eq
    (lambda (target)
      (let* ((ver '5.2.0)
             (src (remote-archive
                    (conc "https://code.call-cc.org/releases/" ver "/chicken-" ver ".tar.gz")
                    "M4edTDnvYOsgcWLxIoMpBTE0bXpG3wS3BmfiHiJ9_uA=")))
        (lambda (host)
          (let* ((copts      (lambda (conf)
                               (append
                                 (without
                                   ;; select optimization with OPTIMIZE_FOR_{SIZE,SPEED};
                                   ;; don't use -fstack-protector* because the
                                   ;; generated C code is all noreturn
                                   '(-fstack-protector
                                      -fstack-protector-strong
                                      -Os -O2 -O3)
                                   ($CFLAGS conf))
                                 ;; necessary for building chicken
                                 '(-fno-strict-aliasing -fwrapv -DHAVE_CHICKEN_CONFIG_H))))
                 (hostarch   ($arch host))
                 (targetarch ($arch target))
                 (buildarch  *this-machine*)
                 (mflags     '(PREFIX=/usr
                                STATICBUILD=1
                                PLATFORM=linux
                                DESTDIR=/out
                                TARGET_RUN_PREFIX=/usr
                                OPTIMIZE_FOR_SPEED=1
                                "LINKER_STATIC_OPTIONS=-static-pie -L."))
                 (kvflags    (k=v*
                               PROGRAM_PREFIX: (conc ($triple target) "-")
                               ARCH: (chicken-arch host)
                               C_COMPILER: ($CC host)
                               CXX_COMPILER: ($CXX host)
                               LIBRARIAN: ($AR host)
                               C_COMPILER_OPTIONS: (copts host)
                               TARGET_C_COMPILER: ($CC target)
                               TARGET_C_COMPILER_OPTIONS: (copts target)
                               TARGET_CXX_COMPILER: ($CXX target)
                               TARGET_LIBRARIAN: ($AR target)
                               TARGET_PREFIX: (conc ($sysroot target) "/usr"))))
            (make-package
              label:  (conc "chicken-" ver "-" ($arch target) "-" ($arch host))
              src:    src
              tools:  (append
                        (cc-for-target host)
                        (if (eq? targetarch hostarch)
                          '()
                          (append (gcc-for-target target) (binutils-for-target target))))
              inputs: (list musl libssp-nonshared)
              build:  (make-recipe
                        script: `((cd ,(conc "chicken-" ver))
                                  (if ((make ,@mflags ,@kvflags chicken-config.h)))
                                  (if ((make ,@mflags ,@kvflags)))
                                  (if ((make ,@mflags ,@kvflags install)))
                                  (rm -rf /out/usr/share))))))))))

;; chicken is a "native" chicken
(define chicken
  (lambda (conf)
    ((chicken-for-target conf) conf)))
