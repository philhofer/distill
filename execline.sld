(define-library (distill execline)
  (export
    write-exexpr
    execline-shebang
    execline-execs
    exec-begin
    execline*)
  (import
    scheme
    (scheme base)
    (scheme write)
    (srfi 4)
    (srfi 13)
    (srfi 26))
  (cond-expand
    (chicken
      (import
        (chicken bitwise)
        (chicken type))))
  (include "execline.scm"))
