(define-library (distill image)
  (export
    squashfs
    initramfs
    ;; packages:
    xz-utils
    lz4
    zstd
    squashfs-tools
    libarchive
    libressl)
  (import
    scheme
    (scheme base)
    (scheme cxr)
    (distill kvector)
    (distill base)
    (distill execline)
    (distill plan)
    (distill package))
  (cond-expand
    (chicken (import
               (only (chicken string) conc)
               (only (chicken port) with-output-to-string))))
  (include "image.scm"))

