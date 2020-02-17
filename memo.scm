
(define-syntax cons*
  (syntax-rules ()
    ((_ head rest)
     (cons head rest))
    ((_ head next rest* ...)
     (cons head (cons* next rest* ...)))))

(define-syntax as
  (syntax-rules ()
    ((_ pred? expr)
     (let ((v expr))
       (assert (pred? v))
       v))))

;; a sigil used for determining if we're stuck in a loop
(define *bad* (list 'in-progress))

;; memoize a single-argument function
;;
;; TODO: something more efficient than an alist?
(: memoize-eq (forall (a b) ((a -> b) -> (a -> b))))
(define (memoize-eq proc)
  (let ((results (make-hash-table test: eq? hash: eq?-hash)))
    (lambda (arg)
      (let ((res (hash-table-ref
                   results arg
                   (lambda ()
                     (hash-table-set! results arg *bad*)
                     (let ((out (proc arg)))
                       (hash-table-set! results arg out)
                       out)))))
        (if (eq? res *bad*)
          (error "cannot perform recursive memoization")
          res)))))

;; memoize-lambda takes an expression of the form
;;   (memoize-lambda (arg0 arg1 ...) body ...)
;; and yields a lambda that only evaluates its body
;; once for each unique set of arguments (note that the
;; arguments are compared using 'eq?')
;;
;; the strategy here is to break up the lambda
;; into a curried functions for each argument from left to right,
;; so each argument gets applied to a function that returns the
;; memoized continuation of the function for that argument
(define-syntax memoize-lambda
  (syntax-rules ()
    ((_ (formals* ...) body* ...)
     (let ((self (memoize-lambda "memoize-args" (formals* ...) body* ...)))
       (lambda (formals* ...)
         (memoize-lambda "real-body" self (formals* ...)))))
    ((_ "memoize-args" () body* ...)
     (begin body* ...))
    ((_ "memoize-args" (formal formals* ...) body* ...)
     (memoize-eq
       (lambda (formal)
         (memoize-lambda "memoize-args" (formals* ...) body* ...))))
    ((_ "real-body" self ())
     self)
    ((_ "real-body" self (formal formals* ...))
     (memoize-lambda "real-body" (self formal) (formals* ...)))))

;; sugar for defining memoized functions
(define-syntax define-memoized
  (syntax-rules ()
    ((_ (name formals* ...) body* ...)
     (define name (memoize-lambda (formals* ...) body* ...)))))
