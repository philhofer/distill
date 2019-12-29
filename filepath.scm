(: foldl1 (forall (a b) ((a b -> a) a (list-of b) -> a)))
(define (foldl1 proc init lst)
  (let loop ((val init)
	     (lst lst))
    (if (null? lst)
        val
	(loop (proc val (car lst)) (cdr lst)))))

(: filepath-join (string #!rest string --> string))
(define (filepath-join first . rest)
  (let* ((cons-part (lambda (out part)
		      ;; push 'part' onto 'out' unless it is
		      ;; a special path component (".." means pop)
		      (cond
			((or (string=? part "") (string=? part "."))
			 out)
			((string=? part "..")
			 (if (or (null? out) (string=? (car out) ".."))
			     (cons part out)
			     (cdr out)))
			(else
			 (cons part out)))))
	 (cons-args (lambda (out arg)
		      ;; push 'arg' components onto 'out'
		      (foldl1 cons-part out (string-split arg "/"))))
	 (rev-parts (foldl1 cons-args '() (cons first rest)))
	 (abspath?  (string-prefix? "/" first))
	 (prepend   (lambda (out part)
		      (string-append part "/" out)))
	 (fullpath  (foldl1 prepend (car rev-parts) (cdr rev-parts))))
    (if abspath?
        (string-append "/" fullpath)
        fullpath)))

(: abspath (string --> string))
(define (abspath p)
  (if (eq? (string-ref p 0) #\/) p (filepath-join (current-directory) p)))

(: foldl-dir (forall (a) ((a string -> a) a string -> a)))
(define (foldl-dir proc seed dir)
  (find-files
    dir
    #:seed seed
    #:test (lambda (f)
	     (memq (file-type f) '(normal-file link)))
    #:dotfiles #t
    #:action (lambda (f val)
	       (proc val f))))
