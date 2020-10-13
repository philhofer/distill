(define-library (distill sandbox)
  (export
    sandbox-run
    with-new-jobserver
    jobserver+
    jobserver-
    call-with-job)
  (import
    scheme
    (srfi 39)
    (distill filepath)
    (distill coroutine)
    (distill eprint))
  (cond-expand
    (chicken (import
               (only (chicken base) include unless
                     when delay-force error let-values)
               (only (chicken condition)
                     current-exception-handler
                     print-error-message)
               (chicken type)
               (chicken file posix)
               (chicken fixnum)
               (chicken process))))
  (include "sandbox.scm"))
