(define-library (distill image)
  (export
    squashfs
    initramfs
    ext2fs
    linux-esp
    diskparts)
  (import
    scheme
    (distill kvector)
    (distill execline)
    (distill plan)
    (distill memo)
    (distill text)
    (distill package)
    (distill filepath)
    (distill base))
  (cond-expand
    (chicken (import
               (only (chicken base) include error unless vector-copy!)
               (only (chicken string) conc)
               (only (chicken port) with-output-to-string))))
  (include "image.scm"))

