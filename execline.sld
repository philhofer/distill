(define-library (distill execline)
  (export
    write-exexpr
    execline-shebang
    exec-begin
    execline*)
  (import
    scheme
    (srfi 4)
    (srfi 13)
    (srfi 26))
  (cond-expand
    (chicken
      (import
        (only (chicken base) include error unless when)
        (chicken bitwise)
        (chicken type))))
  (include "execline.scm"))
