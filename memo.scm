
(define-syntax cons*
  (syntax-rules ()
    ((_ head rest)
     (cons head rest))
    ((_ head next rest* ...)
     (cons head (cons* next rest* ...)))))

(define *bad* (list 'in-progress))

;; memoize a single-argument function
;;
;; TODO: something more efficient than an alist?
(: memoize-eq (forall (a b) ((a -> b) -> (a -> b))))
(define (memoize-eq proc)
  (let ((results '()))
    (lambda (arg)
      (or (and-let* ((p (assq arg results))
		     (v (cdr p)))
	    (if (eq? v *bad*)
	      ;; we are inside a call to this same memoization lambda!
	      (error "cannot perform recursive memoization")
	      v))
	  (let ((cell (cons arg *bad*)))
	    (set! results (cons cell results))
	    (let ((val (proc arg)))
	      (set-cdr! cell val)
	      val))))))

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
