(define-library (distill fs)
  (export
    kmsg
    swapon
    var-mount
    var-mounted-rw)
  (import
    scheme
    (srfi 88)
    (distill plan)
    (distill base)
    (distill service)
    (distill filepath)
    (distill kvector)
    (distill sequence))
  (cond-expand
    (chicken (import
               (only (chicken base) include error foldl)
               (only (chicken string) conc))))
  (include "fs.scm"))
