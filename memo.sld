(define-library (distill memo)
  (import
    scheme
    (srfi 2)
    (srfi 69))
  (cond-expand
    (chicken
      (import
        (only (chicken base) include error)
        (chicken type))))
  (export
    cons*
    memoize-eq
    memoize-one-eq
    memoize-lambda
    define-memoized)
  (include "memo.scm"))
