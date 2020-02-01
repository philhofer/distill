(import
  scheme
  (distill package)
  (distill plan)
  (distill execline)
  (distill buildenv)
  (distill base)
  (only (chicken string) conc))

(define zstd
  (let* ((version '1.4.4)
         (src     (remote-archive
                    (conc "https://github.com/facebook/zstd/archive/v"
                          version
                          ".tar.gz")
                    "PKNr93GxvtI1hA4Oia-Ut7HNNGjAcxlvfSr3TYdpdX4=")))
    (lambda (conf)
      (make-package
        label:  (conc "zstd-" (conf 'arch))
        src:    src
        tools:  (cc-for-target conf)
        inputs: (list musl libssp-nonshared)
        build:
        ;; just a raw Makefile
        (let* ((cenv  (cc-env conf))
               (menv  (make-env conf))
               (extra '((HAVE_PTHREAD . 1)
                        (HAVE_ZLIB . 0)
                        (HAVE_LZMA . 0)
                        (HAVE_LZ4 . 0)
                        (ZSTD_LEGACY_SUPPORT . 0)
                        (ZSTD_LIB_DEPRECATED . 0)))
               (makeflags (map pair->string=
                               (append cenv menv extra))))
          (make-recipe
            script: (execline*
                      (cd ,(conc "zstd-" version))
                      (importas -u "-i" nproc nproc)
                      (if ((cd lib/)
                           (make -j $nproc PREFIX=/usr
                                 DESTDIR=/out ,@makeflags install-static install-includes)))
                      (if ((cd programs/)
                           (make -j $nproc ,@makeflags zstd)))
                      (install -D -m "755"
                               programs/zstd /out/usr/bin/zstd))))))))
