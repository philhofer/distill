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
    (srfi 26)
    (chicken type)
    fmt)
  (include "execline.scm"))
