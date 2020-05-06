(define-library (distill fs)
  (export
    kmsg
    swapon
    var-mount
    var-mounted-rw
    var-log-services)
  (import
    scheme
    (srfi 88)
    (distill plan)
    (distill base)
    (distill text)
    (distill service)
    (distill filepath)
    (distill kvector))
  (cond-expand
    (chicken (import
               (only (chicken base) include error foldl)
               (only (chicken string) conc))))
  (include "fs.scm"))
