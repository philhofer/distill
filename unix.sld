(define-library (distill unix)
  (export
    adduser
    addgroup
    groups+users->artifacts)
  (import
    scheme
    (srfi 26)
    (srfi 69)
    (distill text)
    (distill plan)
    (distill kvector)
    (distill contract))
  (cond-expand
    (chicken (import
               (chicken type)
               (only (chicken base) include error fixnum?))))
  (include "unix.scm"))
