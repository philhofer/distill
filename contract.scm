
(define-syntax conform
  (syntax-rules ()
    ((_ contract value)
     (if (contract value)
       value
       (error "value doesn't conform to contract:" value (quote contract))))))

(define false/c (lambda (arg) (not arg)))
(define true/c  (lambda (arg) arg))
(define any/c   (lambda (arg) #t))

(define (perhaps otherwise)
  (lambda (in)
    (or (not in) (otherwise in))))

;; or/c takes contract parameters and returns a contract
;; that passes when any of its input contracts pass
(: or/c (#!rest (* -> boolean) -> (* -> boolean)))
(define or/c disjoin)

(: cmp/c ((* 'a -> boolean) 'a -> (* -> boolean)))
(define (cmp/c cmp val)
  (lambda (arg)
    (cmp arg val)))

(define (eq?/c val) (cmp/c eq? val))
(define (eqv?/c val) (cmp/c eqv? val))
(define (equal?/c val) (cmp/c equal? val))
(define (string=?/c val) (cmp/c string=? val))
(define (=/c val) (cmp/c = val))

;; and/c takes contract parameters and returns
;; a contract that passes only when *all* of its
;; input contracts pass
(: and/c (#!rest (* -> boolean) -> (* -> boolean)))
(define and/c conjoin)

;; list-of takes a contract parameter and
;; returns a contract that passes when it
;; is given a list where every element passes the
;; input contract
(: list-of ((* -> boolean) -> (* -> boolean)))
(define (list-of c?)
  (lambda (arg)
    (and (list? arg)
         (let loop ((lst arg))
           (or (null? lst)
               (and (c? (car lst))
                    (loop (cdr lst))))))))

(define (pair-of car/c cdr/c)
  (lambda (arg)
    (and (pair? arg)
         (car/c (car arg))
         (cdr/c (cdr arg)))))

;; vector-of is like list-of, but for vectors
(: vector-of ((* -> boolean) -> (* -> boolean)))
(define (vector-of c?)
  (lambda (arg)
    (and (vector? arg)
         (let ((len (vector-length arg)))
           (let loop ((i 0))
             (or (fx>= i len)
                 (and (c? (vector-ref arg i))
                      (loop (fx+ i 1)))))))))

;; vector/c takes input contracts and matches a vector
;; where each vector element matches its corresponding
;; contract argument
;;
;; i.e. (vector/c integer? symbol? string?)
;; matches #(1 'foo "foo")
(: vector/c (#!rest (* -> boolean) -> (* -> boolean)))
(define (vector/c . args)
  (lambda (arg)
    (and (vector? arg)
         (let loop ((i   0)
                    (lst args))
           (if (null? lst)
             (fx= i (vector-length arg))
             (and (fx< i (vector-length arg))
                  ((car lst) (vector-ref arg i))
                  (loop (fx+ i 1) (cdr lst))))))))

(: list/c (#!rest (* -> boolean) -> (* -> boolean)))
(define (list/c . args)
  (lambda (arg)
    (and (list? arg)
         (let loop ((in arg)
                    (ok args))
           (if (null? in)
             (null? ok)
             (and (not (null? ok))
                  ((car ok) (car in))
                  (loop (cdr in) (cdr ok))))))))
