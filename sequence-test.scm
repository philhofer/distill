(import
  (scheme base)
  (srfi 69)
  (chicken type)
  (only (srfi 13) substring/shared))

(include "sequence.scm")
(include "test-helpers.scm")

(let* ((lst '(a b c d))
       (seq (list->seq lst)))
  (test equal? lst (seq->list seq))
  (test equal? (list->vector lst) (seq->vector seq))
  (test equal? (list->vector lst) (seq->vector (vector->seq (list->vector lst))))
  (test eq? #t (any/s? symbol? seq))
  (test eq? #t (all/s? symbol? seq)))

(let* ((lst   '(1 3 5 7 1 3 5))
       (seq   (list->seq lst))
       (add1  (lambda (x) (+ x 1)))
       (seq+1 (s/map add1 seq)))
  (test equal? '(1 3 5 7) (seq->list (s/uniq seq test: = hash: number-hash)))
  (test equal? (map add1 lst) (seq->list seq+1))
  (test equal? '(2 4 6 8) (seq->list (s/uniq seq+1 test: = hash: number-hash)))
  (test equal? '()        (seq->list (s/filter even? seq)))
  (test equal? lst        (seq->list (s/filter odd?  seq)))
  (test equal? '(1 3 5 7) (seq->list (s/filter odd? (s/uniq seq test: = hash: number-hash))))
  (test equal? '(1 3 5 7 1 3 5
                 2 4 6 8 2 4 6) (seq->list (s/append seq seq+1))))

(let* ((str "a/b/c")
       (seq (string-sep->seq str #\/)))
  (test equal? '("a" "b" "c") (seq->list seq))
  (test equal? '("abc") (seq->list (string-sep->seq "abc" #\/))))
