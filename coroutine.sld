(define-library (coroutine)
  (export
    make-semaphore
    semacquire
    semrelease
    with-semaphore
    process-wait/yield
    proc-status
    proc-return
    spawn
    join/value)
  (import
    scheme
    (scheme base)
    (srfi 12)
    (srfi 69)
    (eprint))
  (cond-expand
    (chicken (import
               (chicken type)
               (chicken condition)
               (only (chicken process) process-wait))))
  (include "coroutine.scm"))
