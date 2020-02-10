(define-library (distill linux)
  (export
    linux-headers)
  (import
    scheme
    (scheme base)
    (distill base)
    (distill plan)
    (distill package)
    (distill buildenv)
    (distill execline)
    (only (distill image) xz-utils))
  (cond-expand
    (chicken (import
               (only (chicken string) conc))))
  (include "linux.scm"))
