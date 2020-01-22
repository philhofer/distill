(import
  scheme
  (scheme load)
  (chicken process-context)
  (chicken repl)
  (only (chicken read-syntax)
	set-parameterized-read-syntax!)
  (only (chicken base) vector-resize)
  (execline)
  (hash)
  (filepath)
  (log)
  (plan)
  (base))

;; this is a hack to prevent additional
;; (import ...) statements for execline/hash/filepath/log
;; from having to load any code, since the import library
;; contains the syntactic definitions that we would otherwise
;; need to load when expanding other code
(begin
  (include "memo.import.scm")
  (include "table.import.scm")
  (include "execline.import.scm")
  (include "hash.import.scm")
  (include "filepath.import.scm")
  (include "log.import.scm")
  (include "plan.import.scm")
  (include "base.import.scm"))

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
    (for-each load args)))
