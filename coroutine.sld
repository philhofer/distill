(define-library (distill coroutine)
  (export
    make-semaphore
    semacquire
    semacquire/n
    semacquire/max
    semrelease
    semrelease/n
    with-semaphore
    process-wait/yield
    proc-status
    proc-return
    spawn
    join/value)
  (import
    scheme
    (srfi 11)
    (srfi 12)
    (srfi 39)
    (srfi 69)
    (distill eprint))
  (cond-expand
    (chicken (import
               (chicken type)
               (chicken condition)
               (only (chicken base) include error when call/cc)
               (only (chicken process) process-wait))))
  (include "coroutine.scm"))
