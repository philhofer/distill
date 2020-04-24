(import
  scheme
  (srfi 69)
  (chicken type)
  (only (srfi 13) substring/shared)
  (only (chicken port) call-with-output-string))

(include "sequence.scm")
(include "test-helpers.scm")

(let* ((lst '(a b c d))
       (seq (list->seq lst)))
  (test equal? lst (seq->list seq))
  (test equal? (list->vector lst) (seq->vector seq))
  (test equal? (list->vector lst) (seq->vector (vector->seq (list->vector lst))))
  (test eq? #t (any/s? symbol? seq))
  (test eq? #t (all/s? symbol? seq))
  (test string=? "a\nb\nc\nd\n" (lines/s seq)))

(let* ((lst   '(1 3 5 7 1 3 5))
       (seq   (list->seq lst))
       (add1  (lambda (x) (+ x 1)))
       (seq+1 (s/map add1 seq)))
  (test eq? #t (all/s? odd? seq))
  (test eq? #f (any/s? even? seq))
  (test eq? #t (all/s? even? seq+1))
  (test eq? #f (any/s? odd? seq+1))
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

(let* ((tree '(0 . ((1 . ((3 . ()) . (4 . ()))) . (2 . ((5 . ()) . (6 . ()))))))
       (seq  (list->seq (list tree)))
       (sub  (lambda (p)
               (if (pair? (cdr p))
                 (lambda (kons seed)
                   (kons (cddr p) (kons (cadr p) seed)))
                 empty-seq)))
       (val  (lambda (p)
               (if (pair? p) (car p) p)))
       (kpre  (kompose
                (label self)
                (k/preorder self sub)
                (k/map val)))
       (kpost (kompose
                (label self)
                (k/postorder self sub)
                (k/map val))))
  (test equal? (reverse '(0 1 3 4 2 5 6))
        (seq (kpre cons) '()))
  (test equal? (reverse '(3 4 1 5 6 2 0))
        (seq (kpost cons) '())))

(define (bfs-uniq child)
  (kompose
    (label top)
    (k/uniq test: eq? hash: eq?-hash)
    (k/preorder top child)))

(define (dfs-uniq child)
  (kompose
    (label top)
    (k/uniq test: eq? hash: eq?-hash)
    (k/postorder top child)))

;; test a graph in adjacency-list format
;; which yields itself in reverse in BFS
;; and in ordinary order in DFS traversal
(let* ((graph '#((1 2 3)
                 (2 0 3)
                 (3 1 0)
                 (0 1 2)))
       (seq   (vector->seq graph))
       (child (lambda (lst)
                (s/map (cut vector-ref graph <>) (list->seq lst))))
       (revg  (seq cons '()))
       (kbfs  (bfs-uniq child))
       (kdfs  (dfs-uniq child)))
  (test equal? revg (seq (kbfs cons) '()))
  (test equal? (vector->list graph) (seq (kdfs cons) '())))

(test equal?
      "first\nsecond line\nthird line values\nfourth\n"
      (->lines+spaces
       '(first
	 (second line)
	 (third line values)
	 fourth)))
