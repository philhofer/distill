(cond-expand
  (csi (import r7rs)
       (load "memo.sld"))
  (else (begin)))

(import (distill memo))
(include "test-helpers.scm")

(define-memoized (foo x y) (cons x y))

(define neq? (lambda (x y) (not (eq? x y))))

(let ((first (foo 'x 'y)))
  (test equal? first (cons 'x 'y))
  (test eq? first (foo 'x 'y))
  (let ((second (foo 'y 'x)))
    (test neq? first second)
    (test eq?  second (foo 'y 'x))))
