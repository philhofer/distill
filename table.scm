(: lookup (vector symbol --> *))
(define (lookup tbl sym)
  (let* ((len (vector-length tbl))
	 (h1  (symbol-hash sym len))
	 (ref (lambda (slot)
		(and-let* ((cell (vector-ref tbl slot))
			   (_    (eq? (car cell) sym)))
		  (cdr cell)))))
    (or (ref h1)
	(let ((h2 (symbol-hash sym len h1)))
	  (or (ref h2)
	      (let ((h3 (symbol-hash sym len h2)))
		(ref h3)))))))

;; table->proc turns a table into a single-argument
;; procecudure that returns associations
(: table->proc (vector -> (procedure (symbol) *)))
(define (table->proc tbl)
  (lambda (sym)
    (lookup tbl sym)))

;; table=? compares two tables using elem=?
;; to compare values associated with keys in each table
(: table=? (vector vector (procedure (* *) *) -> *))
(define (table=? left right elem=?)
  (define (super outer inner)
    (let ((len (vector-length outer)))
      (let loop ((i 0))
	(or (>= i len)
	    (let ((cell (vector-ref outer i)))
	      (or (eq? cell #f)
		  (let ((v (lookup inner (car cell))))
		    (and v (elem=? v (cdr cell)) (loop (+ i 1))))))))))
  (and (super left right)
       (super right left)))

;; (insert! table symbol value) creates
;; an association that can be later retreived with (lookup table symbol);
;; inserting the value #f is equivalent to removing that association
;; from the table
(: insert! (vector symbol * -> vector))
(define (insert! tbl sym val)

  ;; grow the table's capacity
  (define (grow! tbl)
    (let ((newvec (make-vector (ceiling (/ (* (vector-length tbl) 3) 2)) #f))
	  (oldlen (vector-length tbl)))
      (let loop ((i 0)
		 (vec newvec))
	(if (>= i oldlen)
	    vec
	    (let ((cell (vector-ref tbl i)))
	      (loop (+ i 1)
		    (if cell (insert! vec (car cell) (cdr cell)) vec)))))))

  ;; maybe insert a value, provided it is a pure overwrite
  (define (insert!? vec slot sym val)
    (let ((curval (vector-ref vec slot)))
      (and (or (not curval)
	       (eq? (car curval) sym))
	   (begin
	     (vector-set! vec slot (if val (cons sym val) val))
	     vec))))

  ;; decide when to grow the table
  (define (cycle? s depth)
    (or (and (>= depth 1)
	     (eq? s sym))
	(>= depth 10)))

  (let loop ((vec   tbl)
	     (sym   sym)
	     (val   val)
	     (depth 0))
    ;; try to insert in 3 probe positions
    (if (cycle? sym depth)
	(loop (grow! vec) sym val 0)
	(let* ((len (vector-length vec))
	       (h1  (symbol-hash sym len)))
	  (or (insert!? vec h1 sym val)
	      (let ((h2 (symbol-hash sym len h1)))
		(or (insert!? vec h2 sym val)
		    (let ((h3 (symbol-hash sym len h2)))
		      (or (insert!? vec h3 sym val)
			  ;; all probes failed, so either we bump the
			  ;; first probe position, or val is #f and we
			  ;; simply return (because a lookup would return #f)
			  (if val
			      (let ((oldval (vector-ref vec h1)))
				(vector-set! vec h1 (cons sym val))
				(loop vec (car oldval) (cdr oldval) (+ depth 1)))
			      vec))))))))))

(define table
  (case-lambda
    (() (make-vector 8 #f))
    ((alist) (let ((vec (make-vector (+ (* (length alist) 2) 1) #f)))
	       (foldl1
		 (lambda (v p)
		   (insert! v (car p) (cdr p)))
		 vec
		 alist)))))

;; tables are just vectors
(define table? vector?)

;; produce an alist of the non-empty cells in the table
;;
;; in other words, (equal? alist (table->alist (table alist)))
(: table->alist (vector -> (list-of (pair symbol *))))
(define (table->alist v)
  (let ((len (vector-length v)))
    (let loop ((i 0)
	       (lst '()))
      (if (>= i len)
	  lst
	  (let ((p (vector-ref v i)))
	    (loop (+ i 1)
		  (if (pair? p)
		      (cons p lst)
		      lst)))))))

(define (foldl1 proc seed lst)
  (let loop ((state seed)
	     (lst   lst))
    (if (null? lst)
	state
	(loop (proc state (car lst)) (cdr lst)))))
