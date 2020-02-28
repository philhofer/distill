;; hack to do lexical comparison of keywords
(: kw<? (keyword keyword --> boolean))
(define (kw<? a b)
  (string<? (##sys#symbol->string a)
            (##sys#symbol->string b)))

(: kidx ((vector-of keyword) keyword --> (or false fixnum)))
(define (kidx vec kw)
  (unless (keyword? kw)
    (error "expected keyword in kidx; got" kw))
  (let loop ((lo 0)
             (hi (vector-length vec)))
    (if (fx>= lo hi)
      #f
      (let* ((mid  (fx+ lo (fx/ (fx- hi lo) 2)))
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
      (error "kvector doesn't use keyword" kw))))

(: kref/default (vector keyword * --> *))
(define (kref/default vec kw val)
  (let ((idx (kidx (vector-ref vec 0) kw)))
    (if idx
      (vector-ref vec idx)
      val)))

(: kref* (vector keyword --> *))
(define (kref* vec kw) (kref/default vec kw #f))

(: kset! (vector keyword * -> undefined))
(define (kset! vec kw arg)
  (let ((idx (kidx (vector-ref vec 0) kw)))
    (if idx
      (vector-set! vec idx arg)
      (error "kvector doesn't use keyword" kw))))

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
            (error "kvector doesn't use keyword" (car args))))))))

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

(: kvector->list (vector --> list))
(define (kvector->list kv)
  (kvector-foldl kv (lambda (k v lst)
                      (cons k (cons v lst))) '()))

(: kvector->alist (vector --> (list-of (pair keyword *))))
(define (kvector->alist kv)
  (kvector-foldl kv (lambda (k v lst)
                      (cons (cons k v) lst)) '()))

;; interned list of kvector types
(define *canon-kwlists* (make-hash-table))

(: %canon-vec (vector -> vector))
(define (%canon-vec vec)
  (or (hash-table-ref/default *canon-kwlists* vec #f)
      (begin
        (hash-table-set! *canon-kwlists* vec vec)
        vec)))

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
         (conform? (lambda (out)
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
            (vector-set! template idx val)
            (vector-set! contract (fx- idx 1) ok?)
            (loop (cdddr args)))))
  (lambda args
    (let ((vec (vector-copy template)))
      (let loop ((args args))
        (if (null? args)
          (begin
            (unless (conform? vec)
              (error "kvector doesn't conform to spec" spec))
            vec)
          (let ((idx (kidx kt (car args))))
            (if idx
              (begin
                (vector-set! vec idx (cadr args))
                (loop (cddr args)))
              (error "keyword not part of kvector:" (car args))))))))))

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

(define (kvector-contract-constructor kt . args)
  (let* ((make      (kvector-constructor kt))
         (contracts (apply make args)))
    (lambda args
      (let ((out (apply make args))
            (len (vector-length contracts)))
        (let loop ((i 1))
          (if (fx>= i len)
            out
            (let ((check (vector-ref contracts i)))
              (when check
                (unless (check (vector-ref out i))
                  (error "predicate failed on field:" (vector-ref kt (fx- i 1)) (vector-ref out i))))
              (loop (fx+ i 1)))))))))

(define (kvector-getter kt kw)
  (let ((idx (kidx kt kw)))
    (if idx
      (lambda (v)
        (vector-ref v idx))
      (error "kvector type does not respond to keyword:" kw))))

(: kvector-setter (vector keyword --> (vector * -> undefined)))
(define (kvector-setter kt kw)
  (let ((idx (kidx kt kw)))
    (if idx
      (lambda (v e)
        (vector-set! v idx e))
      (error "kvector type does not respond to keyword:" kw))))

