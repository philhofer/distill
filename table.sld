(define-library (distill table)
  (export
    table
    table?
    table->proc
    table->alist
    lookup
    insert!)
  (import
    (scheme base)
    (scheme case-lambda)
    (srfi 2)
    (only (srfi 69) symbol-hash))
  (cond-expand
    (chicken
      (import (chicken type))))
  (include "table.scm"))
