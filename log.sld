(define-library (log)
  (export
    trace-log
    trace
    info
    fatal)
  (import
    scheme
    (chicken base) ;; exit, current-error-port
    fmt)
  (include "log.scm"))
