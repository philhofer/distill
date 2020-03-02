(define-library (distill fs)
  (export
    swapon
    var-mount
    var-mounted-rw
    e2fsprogs)
  (import
    scheme
    (only (srfi 13) substring/shared)
    (scheme base)
    (distill plan)
    (distill base)
    (distill package)
    (distill service)
    (only (distill linux) linux-headers)
    (only (distill image) zstd)
    (distill kvector)
    (distill sequence)
    (distill execline))
  (cond-expand
    (chicken (import
               (chicken keyword)
               (only (chicken base) foldl)
               (only (chicken string) conc))))
  (include "fs.scm"))
