(define-library (log)
  (export
    trace-log
    trace
    info
    fatal)
  (import
    (scheme base))
  (cond-expand
    (chicken
      (import
        (only (chicken base) exit current-error-port)
	fmt)))
  (include "log.scm"))
