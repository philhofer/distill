(define-library (distill unix)
  (export
    adduser
    addgroup
    groups+users->artifacts)
  (import
    scheme
    (scheme base)
    (srfi 26)
    (srfi 69)
    (distill plan)
    (distill sequence))
  (cond-expand
    (chicken (import
               (chicken type)
               (typed-records))))
  (include "unix.scm"))
