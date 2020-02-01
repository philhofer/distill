(import
  scheme
  (distill base)
  (distill plan)
  (distill package)
  (distill buildenv)
  (only (chicken string) conc)

  (pkg xz-utils)
  (pkg lz4)
  (pkg zstd)
  (pkg libressl))

(define libarchive
  (let* ((version '3.4.1)
         (src     (remote-archive
                    (conc "https://github.com/libarchive/libarchive/releases/download/v" version "/libarchive-" version ".tar.gz")
                    "dfot337ydQKCCABhpdrALARa6QFrjqzYxFSAPSiflFk=")))
    (lambda (conf)
      (make-package
        label:  (conc "libarchive-" (conf 'arch))
        src:    src
        tools:  (cc-for-target conf)
        inputs: (list bzip2 zlib xz-utils lz4 libressl zstd musl libssp-nonshared)
        build:  (gnu-build
                  (conc "libarchive-" version)
                  (config-prepend conf 'configure-flags
                                  '(--without-xml2 --without-acl --without-attr --without-expat)))))))
