(define-syntax test
  (syntax-rules ()
    ((_ want expr)
     (test equal? want expr))
    ((_ cmp? want expr)
     (let ((val want)
	   (got expr))
       (unless (cmp? val got)
	       (display (quote expr))
	       (newline)
	       (display "should be:\n")
	       (display val)
	       (newline)
	       (display "but got:\n")
	       (display got)
	       (newline)
	       (error "test failure"))))))

(define-syntax test*
  (syntax-rules ()
    ((_ proc ((k vals ...) ...))
     (test* equal? proc ((k vals ...) ...)))
    ((_ cmp? proc ((k vals ...) ...))
     (begin
       (test cmp? k (proc vals ...)) ...))))
