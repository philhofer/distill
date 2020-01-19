(define-library (execline)
  (export
    write-exexpr
    execline-shebang
    execline-execs
    exec-begin
    fmt-execline
    execline*)
  (import
    (scheme write)
    (scheme base)
    (srfi 4)
    (srfi 13)
    (srfi 26))
  (cond-expand
    (chicken
      (import
        (chicken bitwise)
        (chicken type)
	fmt)))
  (include "execline.scm"))
