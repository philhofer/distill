(define-library (distill contract)
  (export
    conform
    and/c
    or/c
    false/c
    true/c
    eq?/c
    eqv?/c
    equal?/c
    string=?/c
    vector-of
    list-of
    pair-of
    vector/c
    list/c)
  (import
    scheme)
  (cond-expand
    (chicken (import
               (only (chicken base) include error)
               (chicken type)
               (chicken fixnum))))
  (include "contract.scm"))
