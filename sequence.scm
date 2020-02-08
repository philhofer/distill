;; a sequence" is really just a lambda
;; that executes the inner loop of a left-fold
;; on some data-structure that it knows how to
;; walk through
;;
;; the advantage to structuring sequence operations
;; this way is that you do not have to do produce
;; intermediate objects when composing operations
;;
;; e.g.
;;
;;  (filter p? (map f lst))
;;
;; allocates an intermediate list, whereas
;;
;;  ((list->seq lst) (f/map f (f/filter p? cons)) '())
;;
;; allocates only one list, and supports prepending
;; itself to another list

(define-type seq (('a 'b -> 'a) 'a -> 'a))

;; list->seq turns a list into a sequence
(: list->seq (list -> seq))
(define (list->seq lst)
  (if (null? lst)
    empty-seq
    (lambda (kons seed)
      (let loop ((lst lst)
                 (out seed))
        (if (null? lst)
          out
          (loop (cdr lst) (kons (car lst) out)))))))

(: seq->list (seq -> list))
(define (seq->list seq)
  (reverse (seq cons '())))

(: vector->seq (vector -> seq))
(define (vector->seq vec)
  (if (= 0 (vector-length vec))
    empty-seq
    (lambda (kons seed)
      (let ((end (vector-length vec)))
        (let loop ((i 0)
                   (out seed))
          (if (>= i end)
            out
            (loop (+ i 1) (kons (vector-ref vec i) out))))))))

(: seq->vector (seq -> vector))
(define (seq->vector seq)
  (list->vector (seq->list seq)))

(: proc->seq ((-> *) -> seq))
(define (proc->seq proc)
  (lambda (iter seed)
    (let loop ((res (proc))
               (out seed))
      (if (eof-object? res)
        out
        (let ((rhs (iter out res)))
          (loop (proc) rhs))))))

(define (k/map proc)
  (lambda (kons)
    (lambda (in out)
      (kons (proc in) out))))

;; k/hash-ref is a reducer that transforms
;; each element by looking up a value in
;; a hash table and then calling the inner
;; reducing function 'kons'
(define (k/hash-ref ht)
  (lambda (kons)
    (lambda (in out)
      (kons (hash-table-ref ht in) out))))

;; s/map takes a procedure and a sequence
;; and yields another sequence that produces
;; items as (proc item) for each item
(: s/map (('x -> 'a) seq -> seq))
(define (s/map proc seq)
  (lambda (kons seed)
    (seq ((k/map proc) kons) seed)))

(define (k/filter pred?)
  (lambda (kons)
    (lambda (in out)
      (if (pred? in)
        (kons in out)
        out))))

;; s/filter takes a predicate and a sequence
;; and yields another sequence that conjoins
;; only items that pass (pred? item)
(: s/filter (('x -> *) seq -> seq))
(define (s/filter pred? seq)
  (lambda (kons seed)
    (seq ((k/filter pred?) kons) seed)))

(define (k/uniq . args)
  (lambda (kons)
    (let ((ht (apply make-hash-table args)))
      (lambda (in out)
        (if (hash-table-ref/default ht in #f)
          out
          (begin
            (hash-table-set! ht in #t)
            (kons in out)))))))

;; s/uniq takes a sequence (and optionally arguments
;; to make-hash-table) and yields a sequence that
;; produces only unique items (duplicate items are filtered out)
;;
;; for example:
;;   (s/uniq seq test: = hash: number-hash)
;; would be suitable for producing unique numbers from a sequence
(define (s/uniq seq . args)
  (let ((k (apply k/uniq args)))
    (lambda (kons seed)
      (seq (k kons) seed))))

(define k/recur
  (lambda (kons)
    (lambda (inseq out)
      (inseq kons out))))

;; s/append takes a list of sequences
;; and produces a sequence that produces all of
;; the items in those sequences in order from left to right
(define (s/append . seqs)
  (let ((inner (list->seq seqs)))
    (lambda (kons seed)
      (inner (k/recur kons) seed))))

;; s/cons* takes a list of arguments,
;; the last of which is a sequence,
;; and produces a new sequence with all of the
;; arguments preceding the last argument prepended
;; to the final (sequence) argument
;;
;; i.e.
;;   (s/cons* x) is equivalent to x
;;   (s/cons* a x) produces (a x ...)
;;   (s/cons* a b x) produces (a b x ...)
;; and so forth
(define (s/cons* first . rest)
  (lambda (kons seed)
    (let loop ((arg  first)
               (rest rest)
               (seed seed))
      (if (null? rest)
        (arg kons seed)
        (loop (car rest)
              (cdr rest)
              (kons arg seed))))))

;; s/any? takes a predicate and a sequence
;; and returns whether or not any of the items
;; in the sequence satisfy (pred? item)
(define (any/s? pred? seq)
  (seq (lambda (item out)
         (or out (pred? item))) #f))

;; s/all? takes a predicate and a sequence
;; and returns whether or not all of the items
;; in the sequence satisfy (pred? item)
(define (all/s? pred? seq)
  (seq (lambda (item out)
         (and out (pred? item))) #t))

;; for-each/s calls (proc item) for each item
;; in seq and returns (void)
;;
;; it is equivalent to (but more efficient than):
;; (for-each proc (reverse (seq cons '())))
(define (for-each/s proc seq)
  (let ((kons (lambda (item out)
                (proc item)
                (void))))
    (seq kons (void))))

;; lines/s produces a string from a sequence
;; by applying (display item) (newline) to
;; each item and collecting the output as a string
(define (lines/s seq)
  (call-with-output-string
    (lambda (prt)
      (seq (lambda (in out)
             (display in out)
             (newline out)
             out)
           prt))))

;; string-sep->seq produces a sequence
;; of strings that are the components of 'str'
;; separated by the character 'sep'
(define (string-sep->seq str sep)
  (lambda (kons seed)
    (let* ((end  (string-length str))
           (scan (lambda (start)
                   (let loop ((i start))
                     (cond
                       ((>= i end) end)
                       ((eqv? (string-ref str i) sep) i)
                       (else (loop (+ i 1))))))))
      (let loop ((i 0)
                 (v seed))
        (if (>= i end)
          v
          (let ((seg (scan i)))
            (loop (+ seg 1) (kons (substring/shared str i seg) v))))))))

(define empty-seq (lambda (kons seed) seed))

(define (app flst obj)
  (if (null? flst)
    obj
    ((car flst) (app (cdr flst) obj))))

;; k/preorder does recursive pre-order traversal
;; using (child node) to produce the sequence
;; and 'inner' as the inner reducer function
(define (k/preorder inner child)
  (lambda (kons)
    (lambda (in out)
      ((child in) inner (kons in out)))))

;; k/postorder does recursive post-order traversal
;; using (child node) to produce the sequence of
;; children at each node and 'inner' as the
;; reducing function for the children
(define (k/postorder inner child)
  (lambda (kons)
    (lambda (in out)
      (kons in ((child in) inner out)))))

(define-syntax kompose
  (syntax-rules (label)
    ((_ "recur" kvar last)
     (last kvar))
    ((_ "recur" kvar (label sym) rest* ...)
     ;; we need the explicit lambda here in order
     ;; to ensure that uses of 'sym' within the rest
     ;; of the macro expansion are not (void) when
     ;; they are evaluated
     (letrec* ((sym (lambda (in out) (val in out)))
               (val (kompose "recur" kvar rest* ...)))
        val))
    ((_ "recur" kvar xfrm rest* ...)
     (xfrm (kompose "recur" kvar rest* ...)))
    ((_ head rest* ...)
     (lambda (kons)
       (kompose "recur" kons head rest* ...)))))

