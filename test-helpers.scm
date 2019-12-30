(define-syntax test
  (syntax-rules ()
    ((_ want expr)
     (test equal? want expr))
    ((_ cmp? want expr)
     (let ((val want)
	   (got expr))
       (unless (cmp? val got)
	 (error "test failure:" (quote expr) "should be" val "but is" got))))))

