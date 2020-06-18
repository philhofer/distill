;; hack to do lexical comparison of keywords
(: kw<? (keyword keyword --> boolean))
(define (kw<? a b)
  (string<? (##sys#symbol->string a)
            (##sys#symbol->string b)))

(: vector-map (('a -> 'b) (vector-of 'a) -> (vector-of 'b)))
(define (vector-map proc vec)
  (let* ((len (vector-length vec))
         (out (make-vector len)))
    (let loop ((i 0))
      (if (fx>= i len)
        out
        (begin
          (vector-set! out i (proc (vector-ref vec i)))
          (loop (fx+ i 1)))))))

(: vector-copy (vector -> vector))
(define (vector-copy vec)
  (let ((out (make-vector (vector-length vec))))
    (vector-copy! vec out)
    out))

(: kidx ((vector-of keyword) keyword --> (or false fixnum)))
(define (kidx vec kw)
  (unless (keyword? kw)
    (error "expected keyword in kidx; got" kw))
  (let loop ((lo 0)
             (hi (vector-length vec)))
    (if (fx>= lo hi)
      #f
      (let* ((mid  (fx+ lo (fxshr (fx- hi lo) 1)))
             (mval (vector-ref vec mid)))
        (cond
          ((eq? mval kw)  (fx+ mid 1))
          ((kw<? mval kw) (loop (fx+ mid 1) hi))
          (else           (loop lo mid)))))))

(: kref (vector keyword --> *))
(define (kref vec kw)
  (let ((idx (kidx (vector-ref vec 0) kw)))
    (if idx
      (vector-ref vec idx)
      (error "kref: kvector doesn't use keyword" kw))))

(: kref/default (vector keyword * --> *))
(define (kref/default vec kw val)
  (let ((idx (kidx (vector-ref vec 0) kw)))
    (if idx
      (vector-ref vec idx)
      val)))

(: kref* (vector keyword --> *))
(define (kref* vec kw) (kref/default vec kw #f))

(: kset! (vector keyword * -> vector))
(define (kset! vec kw arg)
  (let ((idx (kidx (vector-ref vec 0) kw)))
    (if idx
      (begin (vector-set! vec idx arg) vec)
      (error "kset!: kvector doesn't use keyword" kw))))

;; kupdate performs a functional update of 'vec'
;; by producing a copy with the arguments in
;; 'args' as the new values for those fields
(: kupdate (vector #!rest * -> vector))
(define (kupdate vec . args)
  (let ((cp (vector-copy vec)))
    (let loop ((args args))
      (if (null? args)
        cp
        (let ((idx (kidx (vector-ref vec 0) (car args))))
          (if idx
            (begin
              (vector-set! cp idx (cadr args))
              (loop (cddr args)))
            (error "kupdate: kvector doesn't use keyword" (car args))))))))

(define (+= . args)
  (lambda (prev)
    (cond
      ((list? prev)
       (apply append prev args))
      ((not prev)
       (apply append args))
      (else
        (cons prev (apply append args))))))

(define (:= x)
  (lambda (arg) x))

(define (?= x)
  (lambda (arg) (or arg x)))

(define (kwith vec . args)
  (let ((cp (vector-copy vec))
        (kt (vector-ref vec 0)))
    (let loop ((args args))
      (if (null? args)
        cp
        (let ((idx (kidx kt (car args))))
          (if idx
            (begin
              (vector-set! cp idx ((cadr args) (vector-ref vec idx)))
              (loop (cddr args)))
            (error "kwith: kvector doesn't use keyword" (car args) kt)))))))

;; kvector-foldl folds (proc key value seed)
;; over each field in the kvector
(: kvector-foldl (vector (keyword * 'a -> 'a) 'a -> 'a))
(define (kvector-foldl kv proc seed)
  (let* ((kl  (vector-ref kv 0))
         (len (vector-length kl)))
    (let loop ((j 0)
               (val seed))
      (if (fx>= j len)
        val
        (let ((kw (vector-ref kl j))
              (j  (fx+ j 1)))
          (loop j (proc kw (vector-ref kv j) val)))))))

;; kvector-map creates a list from a kvector
;; by applying (proc key value) to each kvector key-value pair
(: kvector-map (vector (keyword * -> 'a) -> (list-of 'a)))
(define (kvector-map kv proc)
  (kvector-foldl
    kv
    (lambda (kw val lst)
      (cons (proc kw val) lst))
    '()))

;; kvector->list converts a kvector into a list
;; in which the even elements are keywords and
;; the odd elements are values
;;
;; (this is the reverse operation of list->kvector)
(: kvector->list (vector --> list))
(define (kvector->list kv)
  (kvector-foldl kv (lambda (k v lst)
                      (cons k (cons v lst))) '()))

(: kvector->alist (vector --> (list-of (pair keyword *))))
(define (kvector->alist kv)
  (kvector-map kv cons))

;; interned list of kvector types
(define *canon-kwlists* (make-hash-table))

(: %canon-vec (vector -> vector))
(define (%canon-vec vec)
  (or (hash-table-ref/default *canon-kwlists* vec #f)
      (begin
        (hash-table-set! *canon-kwlists* vec vec)
        vec)))

;; kvector? determines whether or not an object
;; was created with kvector*, list->kvector, or
;; a procedure returned by kvector-constructor
(: kvector? (* --> boolean))
(define (kvector? x)
  (and (vector? x)
       (fx>= (vector-length x) 1)
       (hash-table-ref/default *canon-kwlists* (vector-ref x 0) #f)))

;; list->kvector turns a list of the form (keyword: value ...)
;; into a kvector
(: list->kvector (list -> vector))
(define (list->kvector lst)
  (let ((kws (let loop ((out '())
                        (lst lst))
               (if (null? lst)
                 out
                 (loop (cons (car lst) out) (cddr lst))))))
    (let ((typ (apply make-kvector-type kws)))
      (apply
        (kvector-constructor typ)
        lst))))

;; kvector* takes parameters of the form (key: value ...)
;; and produces a new kvector
(: kvector* (* -> vector))
(define (kvector* . args)
  (list->kvector args))

;; ktype returns the opaque type of the kvector v
;; (which is an object of the same type that is
;; returned from make-kvector-type)
(: ktype (vector --> vector))
(define (ktype v)
  (vector-ref v 0))

;; make-kvector-type creates a kvector type object
;; from a list of keywords
(: make-kvector-type (#!rest keyword -> vector))
(define (make-kvector-type . kws)
  (%canon-vec
    (let ((vec (list->vector kws)))
      (sort! vec kw<?)
      vec)))

;; kvector-union! takes all of the false fields
;; in 'kv' and replaces them with the corresponding
;; fields in 'default' (both arguments must be of
;; the same type)
(: kvector-union! (vector vector -> vector))
(define (kvector-union! kv default)
  (unless (eq? (ktype kv) (ktype default))
    (error "kvector-union!: different types supplied:" kv default))
  (let ((len (vector-length kv)))
    (let loop ((i 1))
      (if (fx>= i len)
        kv
        (let ((cell (vector-ref kv i)))
          (unless cell
            (vector-set! kv i (vector-ref default i)))
          (loop (fx+ i 1)))))))

;; recast takes a kvector type descriptor and an
;; arbitrary kvector and returns a new kvector
;; typed according to 'rtd' but with keys corresponding
;; to those in 'kv' where they overlap with the type descriptor
;; (effectively, this is kvector "duck typing")
(: recast (vector vector -> vector))
(define (recast rtd kv)
  (if (eq? (ktype kv) rtd)
    kv
    (let* ((len (vector-length rtd))
           (new (make-vector (fx+ len 1) #f)))
      (vector-set! new 0 rtd)
      (let loop ((i 0))
        (if (fx>= i len)
          new
          (let ((kw (vector-ref rtd i))
                (i  (fx+ i 1)))
            (vector-set! new i (kref* kv kw))
            (loop i)))))))

;; kvector-predicate takes a kvector type
;; (from make-kvector-type) and returns
;; the predicate function for that type
(define (kvector-predicate kt)
  (lambda (x)
    (and (vector? x)
         (fx>= (vector-length x) 1)
         (eq? (ktype x) kt))))

;; kvector-constructor takes a kvector type
;; (from make-kvector-type) and returns
;; the constructor for that type
(: kvector-constructor (vector #!rest * --> (#!rest * -> vector)))
(define (kvector-constructor kt . spec)
  (let* ((ktlen    (vector-length kt))
         (template (make-vector (fx+ ktlen 1) #f))
         (contract (make-vector ktlen #f))
         (conform? (lambda (out contract)
                     (let loop ((i 0))
                       (or (fx>= i ktlen)
                           (let ((c (vector-ref contract i))
                                 (i (fx+ i 1)))
			     (and (or (not c)
				      (c (vector-ref out i)))
				  (loop i))))))))
    (vector-set! template 0 kt)
    (let loop ((args spec))
      (or (null? args)
          (let ((idx (kidx kt (car args)))
                (val (cadr args))
                (ok? (caddr args)))
	    (unless (procedure? ok?)
	      (error "kvector-constructor: not a contract" ok?))
            (vector-set! template idx val)
            (vector-set! contract (fx- idx 1) ok?)
            (loop (cdddr args)))))
    (lambda args
      (let ((vec (vector-copy template)))
	(let loop ((args args))
	  (if (null? args)
	      (if (conform? vec contract)
		  vec
		  (error "kvector doesn't conform to spec" spec))
	      (let ((idx (kidx kt (car args))))
		(if idx
		    (begin
		      (vector-set! vec idx (cadr args))
		      (loop (cddr args)))
		    (error "kvector-constructor: keyword not part of kvector:" (car args))))))))))

;; subvector-constructor takes a kvector type
;; and a list of keywords and produces a function
;; that efficiently extracts the list of keywords
;; from kvectors of the given type
(define (subvector-constructor kt . kws)
  (let* ((subv (apply make-kvector-type kws))
         (lidx (vector-map
                 (lambda (kw)
                   (kidx kt kw))
                 subv))
         (len  (vector-length subv)))
    (lambda (kv)
      (let* ((len+1 (fx+ len 1))
             (vec   (make-vector len+1 #f)))
        (vector-set! vec 0 subv)
        (let loop ((i 0))
          (if (fx>= i len)
            vec
            (let ((e (vector-ref lidx i))
                  (i (fx+ i 1)))
              (vector-set! vec i (vector-ref kv e))
              (loop i))))))))

;; kvector/c produces a contract for a kvector
;; of the form (kvector/c <type> <key:> <contract> ... )
;; where the type and key contracts are checked against the input
(define (kvector/c kt . args)
  (let ((proto (apply (kvector-constructor kt) args))
        (pred  (kvector-predicate kt)))
    (lambda (in)
      (and (pred in)
           (let ((len (vector-length proto)))
             (let loop ((i 1))
               (or (fx>= i len)
                   (let ((c? (vector-ref proto i)))
                     (and (or (not c?)
                              (c? (vector-ref in i)))
                          (loop (fx+ i 1)))))))))))

;; keys/c matches a kvector with the given keyword arguments
;;
;; i.e. (keys/c name: date:)
;; will match any kvector with 'name:' and 'date:' fields
(define (keys/c . args)
  (let* ((ktd   (apply make-kvector-type args))
         (pred? (kvector-predicate ktd)))
    (lambda (arg)
      (or (pred? arg)
          (let* ((len   (vector-length ktd))
                 (ind   (vector-ref arg 0))
                 (inlen (vector-length ind)))
            (let loop ((i 0) (j 0))
              (or (fx>= i len)
                  (and (fx< j inlen)
                       (let ((kw (vector-ref ktd i))
                             (nx (vector-ref ind j)))
                         (cond
                           ((not (keyword? nx))
                            (error "expected a keyword; found:" nx))
                           ((eq? kw nx)
                            (loop (fx+ i 1) (fx+ j 1)))
                           ((kw<? nx kw)
                            (loop i (fx+ j 1)))
                           (else #f)))))))))))

;; kvector-getter takes a kvector type (from make-kvector-type)
;; and a keyword and returns a function that takes a kvector of
;; the given type and returns its corresponding entry for that keyword
(: kvector-getter (vector keyword -> (vector -> *)))
(define (kvector-getter kt kw)
  (let ((idx (kidx kt kw)))
    (if idx
      (lambda (v)
        (if (eq? (vector-ref v 0) kt)
          (vector-ref v idx)
          (error "kvector-getter: bad input type" v)))
      (error "kvector type does not respond to keyword:" kw))))

;; kvector-setter takes a kvector type (from make-kvector-type)
;; and a keyword and returns a function that takes a kvector
;; of the given type and a value and sets the corresponding
;; entry in the vector to the value
(: kvector-setter (vector keyword --> (vector * -> undefined)))
(define (kvector-setter kt kw)
  (let ((idx (kidx kt kw)))
    (if idx
      (lambda (v e)
        (if (eq? (vector-ref v 0) kt)
          (vector-set! v idx e)
          (error "kvector-setter: bad input type" v)))
      (error "kvector type does not respond to keyword:" kw))))

;; syntactic sugar for defining a kvector type
(define-syntax define-kvector-type
  (syntax-rules ()
    ((_ "splat" (head* ...) (form* ...))
     (head* ... form* ...))
    ((_ "splat" (head* ...) (form* ...) rest* ...)
     (define-kvector-type "splat" (head* ... form* ...) rest* ...))
    ((_ "constructor" make type (kw default pred) ...)
     (define make
       (define-kvector-type "splat" (kvector-constructor type) (kw default pred) ...)))
    ((_ "getters" type (getter kw) ...)
     (begin
       (define getter (kvector-getter type kw)) ...))
    ((_ type make pred? (kw default pred) ...)
     (begin
       (define type (make-kvector-type kw ...))
       (define pred? (kvector-predicate type))
       (define-kvector-type "constructor" make type (kw default pred) ...)))
    ((_ type make pred? (getter kw default pred) ...)
     (begin
       (define type (make-kvector-type kw ...))
       (define pred? (kvector-predicate type))
       (define-kvector-type "constructor" make type (kw default pred) ...)
       (define-kvector-type "getters" type (getter kw) ...)))))
