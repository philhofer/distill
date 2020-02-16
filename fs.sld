(define-library (distill fs)
  (export
    swapon
    var-mount
    e2fsprogs)
  (import
    scheme
    (only (srfi 13) substring/shared)
    (scheme base)
    (distill plan)
    (distill base)
    (distill buildenv)
    (distill package)
    (distill service)
    (only (distill linux) linux-headers)
    (distill sequence)
    (distill execline))
  (cond-expand
    (chicken (import
               (only (chicken string) conc))))
  (include "fs.scm"))
