(define execline-shebang "#!/bin/execlineb -P")

(: tabs (fixnum --> string))
(define (tabs n)
  (cond
    ;; fast-paths: empty string or a small
    ;; number of tabs become constant string refs
    ((eqv? n 0) "")
    ((<= n 8) (let ((v "\t\t\t\t\t\t\t\t"))
		(substring/shared v (- 8 n))))
    (else (string-append "\t" (tabs (- n 1))))))

;; fmt-execline produces a formatting combinator
;; from the list representation of an execline script
(define (fmt-execline lst)
  (define (join-cmds lst indent)
    (fmt-join
      (lambda (v)
	(cat (tabs indent) (join-arg (car v) (cdr v) indent)))
      lst
      "\n"))
  (define (join-arg arg rest indent)
    (if (list? arg)
        (cat "{\n"
	     (join-cmds arg (+ indent 1))
	     "\n"
	     (tabs indent)
	     "}"
	     (if (null? rest) fmt-null " "))
	(if (null? rest)
	    (dsp arg)
	    (cat (dsp arg) " " (join-arg (car rest) (cdr rest) indent)))))
  (fmt-join/suffix
    (lambda (v)
      (join-arg (car v) (cdr v) 0))
    lst
    "\n"))

(define (foldl1 proc init lst)
  (let loop ((val init)
	     (lst lst))
    (if (null? lst)
	val
	(loop (proc val (car lst)) (cdr lst)))))

;; execline script to string helper
(: exexpr->string (list --> string))
(define (exexpr->string expr)
  (fmt #f 
       (cat
	 execline-shebang
	 "\n"
	 (fmt-execline expr))))

;; execline script 'write' helper
(: write-exexpr (list output-port -> undefined))
(define (write-exexpr expr prt)
  (fmt prt
       (cat
	 execline-shebang
	 "\n"
	 (fmt-execline expr))))

;; execline* is a macro that quasiquotes
;; execline scripts as s-expressions
;;
;; The actual implementation of execline*
;; is just (quasiquote ...), but the
;; syntax-rules macro guarantees that
;; we only match (lexically) valid scripts.
(define-syntax execline*
  ;; the implementation here is really just
  ;; (quasiquote (args ...)), but we enforce
  ;; that each argument have the form (cmd args* ...)
  (syntax-rules ()
    ((_ (cmd args* ...) ...)
     (quasiquote ((cmd args* ...) ...)))))

;; execline-begin wraps a series of execline expressions
;; in if { } blocks so that they execute in sequence
(define-syntax exec-begin
  (syntax-rules ()
    ((_ (cmd args* ...) ... (final fargs* ...))
     (quasiquote
       ((if ((cmd args* ...))) ... (final fargs* ...))))))

;; fold-exexpr folds (fn init line)
;; for every subprogram within
;; an execline expression
(define (fold-exexpr fn init expr)
  (if (null? expr)
      init
      (let ((line (car expr))
	    (rest (cdr expr)))
	(fold-exexpr
	  fn
	  ;; recursively walk arguments
	  ;; that are lists
	  (foldl1
	    (lambda (acc arg)
	      (if (list? arg)
		  (fold-exexpr fn acc arg)
		  acc))
	    (fn init line)
	    (cdr line))
	  rest))))

;; execline-execs produces the list of binaries
;; executed from an execline script
(define (execline-execs expr)
  (fold-exexpr (lambda (lst e)
		 (cons (car e) lst)) '() expr))

