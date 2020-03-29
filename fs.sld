(define-library (distill fs)
  (export
    kmsg
    swapon
    var-mount
    var-mounted-rw
    ext2fs
    e2fsprogs)
  (import
    scheme
    (only (srfi 13) substring/shared)
    (srfi 88)
    (distill plan)
    (distill base)
    (distill package)
    (distill service)
    (distill filepath)
    (only (distill linux) linux-headers)
    (only (distill image) zstd)
    (distill kvector)
    (distill sequence))
  (cond-expand
    (chicken (import
               (only (chicken base) include error foldl)
               (only (chicken string) conc))))
  (include "fs.scm"))
