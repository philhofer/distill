(import
  scheme
  (scheme load)
  (chicken process-context)
  (chicken repl)
  (only (chicken read-syntax)
	set-parameterized-read-syntax!)
  (only (chicken base) vector-resize))

(import-for-syntax
  (only (chicken string) conc))

;; this registers the following units as compiled modules
;; (and declares that they should be linked in) without
;; pulling them into the top-level environment
(let-syntax ((include-imports
               (er-macro-transformer
                 (lambda (expr rename cmp)
                   (cons 'begin
                         (map
                           (lambda (sym)
                             `(begin
                                (include ,(conc sym ".import.scm"))
                                (declare (uses ,sym))))
                           (cdr expr)))))))
  (include-imports
    memo
    execline
    hash
    filepath
    eprint
    plan
    package
    base))

(let ((args (command-line-arguments)))
  (if (null? args)
    (let* ((nv    0)
	   (sav   (vector #f #f #f #f))
	   (push! (lambda (v)
		    (when (not (eq? v (void)))
		      (when (>= nv (vector-length sav))
			(let ((newn (* nv 2)))
			  (set! sav (vector-resize sav newn #f))
			  (set! nv newn)))
		      (vector-set! sav nv v)
		      (set! nv (+ nv 1)))))
	   (ref   (lambda (n)
		    (if (< n nv)
		      (vector-ref sav n)
		      (void))))
	   (%eval (lambda (expr)
		    (let ((res (eval expr)))
		      (push! res)
		      res))))
      (set-parameterized-read-syntax!
	#\!
	(lambda (port number)
	  (list ref number)))
      (repl-prompt
	(lambda ()
	  (string-append "#" (number->string nv) "! = ")))
      (repl %eval))

    ;; when invoked as 'sysplan <foo.scm> args ...'
    ;; load the first argument with the remaining args
    ;; as the command-line-arguments
    (parameterize ((program-name           (car args))
                   (command-line-arguments (cdr args)))
      (load (car args)))))
