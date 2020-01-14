(define-library (memo)
  (import
    (scheme base)
    (srfi 2))
  (cond-expand
    (chicken
      (import
        (chicken type))))
  (export
    cons*
    memoize-eq
    memoize-lambda
    define-memoized)
  (include "memo.scm"))
