(define-library (distill tai64)
  (export
    tai64n-now
    unix->tai64n
    tai64n->unix
    string->tai64n
    tai64n->string)
   ;tai64n->nanoseconds
  (import
    scheme
    (srfi 4)
    (srfi 11)
    (srfi 13))
  (cond-expand
    (chicken (import
               (only (chicken base) include error quotient&remainder quotient&modulo)
               (chicken type)
               (chicken foreign))))
  (include "tai64.scm"))
