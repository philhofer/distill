(define-library (distill eprint)
  (export
    trace-log
    trace
    info
    fatal)
  (import
    scheme
    (srfi 39))
  (cond-expand
    (chicken
      (import
        (only (chicken base) include exit current-error-port when))))
  (include "eprint.scm"))
