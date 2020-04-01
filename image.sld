(define-library (distill image)
  (export
    squashfs
    initramfs
    ext2fs)
  (import
    scheme
    (distill kvector)
    (distill execline)
    (distill plan)
    (distill package)
    (distill filepath)
    (distill base))
  (cond-expand
    (chicken (import
               (only (chicken base) include error unless)
               (only (chicken string) conc)
               (only (chicken port) with-output-to-string))))
  (include "image.scm"))

