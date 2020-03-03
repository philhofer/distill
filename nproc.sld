(define-library (distill nproc)
  (export
    nproc)
  (import
    scheme)
  (cond-expand
    (chicken (import
               (only (chicken base) include error)
               (chicken foreign))))
  (include "nproc.scm"))
