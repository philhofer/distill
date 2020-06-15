(define-library (distill execline)
  (export
    elif
    elif*
    el=
    elconc
    elpath
    elexpand
    eltemplate
    write-exexpr
    execline-shebang)
  (import
    scheme
    (srfi 4)
    (srfi 26)
    (srfi 88)
    (distill kvector)
    (distill filepath)
    (distill text))
  (cond-expand
    (chicken
     (import
       (only (chicken port) with-output-to-string)
       (only (chicken base) include error unless when intersperse)
       (chicken bitwise)
       (chicken type))))
  (include "execline.scm"))
