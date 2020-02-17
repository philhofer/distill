(define-library (distill memo)
  (import
    (scheme base)
    (srfi 2)
    (srfi 69))
  (cond-expand
    (chicken
      (import
        (chicken type))))
  (export
    as
    cons*
    memoize-eq
    memoize-lambda
    define-memoized)
  (include "memo.scm"))
