(define-library (execline)
  (export
    exexpr->string
    write-exexpr
    execline-shebang
    execline-execs
    exec-begin
    fmt-execline
    execline*)
  (import
    (scheme write)
    (scheme base)
    (srfi 13)
    (srfi 26))
  (cond-expand
    (chicken
      (import
        (chicken type)
	fmt)))
  (include "execline.scm"))
