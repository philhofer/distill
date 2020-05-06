(define-library (distill coroutine)
  (export
    push-exception-wrapper
    with-cleanup
    process-wait/yield
    proc-status
    proc-return
    spawn
    with-spawn
    fdwrite
    fdread
    fdpipe
    fdclose
    join/value)
  (import
    scheme
    (srfi 4)
    (srfi 11)
    (srfi 12)
    (srfi 39)
    (srfi 69)
    (distill eprint))
  (cond-expand
    (chicken (import
               (only (chicken gc) gc)
               (chicken type)
               (chicken condition)
               (chicken foreign)
               (chicken fixnum)
               (only (chicken base) include error when call/cc identity
                     define-constant)
               (only (chicken process) process-wait))))
  (include "coroutine.scm"))
