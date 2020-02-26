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
    (distill kvector)
    (distill contract)
    (distill sequence))
  (cond-expand
    (chicken (import
               (chicken type)
               (only (chicken base) fixnum?))))
  (include "unix.scm"))
