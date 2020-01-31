(define-library (distill nproc)
  (export
    nproc)
  (import
    scheme
    (scheme base))
  (cond-expand
    (chicken (import (chicken foreign))))
  (include "nproc.scm"))
