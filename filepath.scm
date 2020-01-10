(: foldl1 (forall (a b) ((a b -> a) a (list-of b) -> a)))
(define (foldl1 proc init lst)
  (let loop ((val init)
	     (lst lst))
    (if (null? lst)
        val
	(loop (proc val (car lst)) (cdr lst)))))

(define-type stringy (or string symbol integer))

(: stringify (stringy --> string))
(define (stringify x)
  (cond
    ((string? x) x)
    ((symbol? x) (symbol->string x))
    ((integer? x) (number->string x 10))
    (else (error "can't stringify" x))))

(: dirname (string --> string))
(define (dirname str)
  (let ((end (- (string-length str) 1)))
    (let loop ((i end))
      (cond
	((<= i 0) (if (eqv? (string-ref str 0) #\/) "/" "."))
	((char=? (string-ref str i) #\/) (substring/shared str 0 i))
	(else (loop (- i 1)))))))

(: basename (string --> string))
(define (basename str)
  (let ((end (- (string-length str) 1)))
    (let loop ((i end))
      (cond
	((< i 0) str)
	((char=? (string-ref str i) #\/) (substring/shared str (+ i 1)))
	(else (loop (- i 1)))))))

;; core filepath normalization routine
;;
;; join one or more filepath components together
;; while eliminating '.' and '..' components where possible
(: filepath-join (stringy #!rest stringy --> string))
(define (filepath-join first . rest)
  ;; scan for '/'
  (define (scan str start)
    (let ((len (string-length str)))
      (let loop ((i start))
	(cond
	  ((>= i len) len)
	  ((eqv? (string-ref str i) #\/) i)
	  (else (loop (+ i 1)))))))
  ;; foldl, but for subsections of 'str'
  ;; that are delimited by '/'
  (define (foldl-parts proc init str)
    (let ((end (string-length str)))
      (let loop ((i 0)
		 (v init))
	(if (>= i end)
	  v
	  (let ((seg (scan str i)))
	    (loop (+ seg 1) (proc v (substring/shared str i seg))))))))
  (let* ((input     (cons first rest))
	 (cons-part (lambda (out part)
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
	 (cons-arg  (lambda (out arg)
		      (foldl-parts cons-part out (stringify arg))))
	 (rev-parts (foldl1 cons-arg '() input))
	 (abspath?  (eqv? (string-ref (stringify first) 0) #\/))
	 (prepend   (lambda (out part)
		      (string-append part "/" out)))
	 (fullpath  (foldl1 prepend (car rev-parts) (cdr rev-parts))))
    (if abspath?
        (string-append "/" fullpath)
        fullpath)))

;; convert a relative path to an absolute path
;; if it is not one already (by prepending the current directory)
(: abspath (string --> string))
(define (abspath p)
  (if (eq? (string-ref p 0) #\/) p (filepath-join (current-directory) p)))
