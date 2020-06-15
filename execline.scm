(define execline-shebang "#!/bin/execlineb -P")

(: tabs (fixnum --> string))
(define (tabs n)
  (cond
    ;; fast-paths: empty string or a small
    ;; number of tabs become constant string refs
    ((eqv? n 0) "")
    ((<= n 8) (let ((v "\t\t\t\t\t\t\t\t"))
                (##sys#substring v 0 n)))
    (else (string-append "\t" (tabs (- n 1))))))

(: %dsp-string (string --> undefined))
(define (%dsp-string obj)
  (let ((len (string-length obj)))
    (if (= len 0)
      (write obj)
      (let loop ((i 0))
        (if (= i len)
          (display obj)
          (case (string-ref obj i)
            ((#\space #\newline #\tab #\linefeed #\\ #\# #\")
             ;; any obvious escape sequences or semantic characters
             ;; mean we encode the string into the script just
             ;; as it would appear as a scheme literal
             (write obj))
            ((#\delete #\backspace #\alarm #\vtab #\nul #\esc)
             ;; while we're here, warn about illegal characters
             (error "illegal character in execline string:" obj))
            (else (loop (+ i 1)))))))))

;; given a bytevector, produce a quoted string
;; that escapes non-printable ascii chars into
;; escape sequences that execlineb will interpret
;; properly
(define (quote-bv bv)
  (let ((len    (u8vector-length bv))
        (->int  (lambda (c)
                  (if (integer? c) c (char->integer c))))
        (->hex1 (lambda (i)
                  (string-ref "0123456789abcdef"
                              (bitwise-and i 15))))
        (->hex0 (lambda (i)
                  (string-ref "0123456789abcdef"
                              (bitwise-and
                                (arithmetic-shift i -4) 15)))))
    (list->string
      (cons #\"
            (let loop ((i 0))
              (if (= i len)
                (list #\")
                (let ((v (->int (u8vector-ref bv i))))
                  (define-syntax cons*
                    (syntax-rules ()
                      ((_ x y) (cons x y))
                      ((_ x y rest* ...) (cons x (cons* y rest* ...)))))
                  (if (<= 32 v 126)
                    (cons (integer->char v) (loop (+ i 1)))
                    (cons*
                      #\\ #\0 #\x (->hex0 v) (->hex1 v)
                      (loop (+ i 1)))))))))))

;; fmt-execline produces a formatting combinator
;; from the list representation of an execline script
(define (dsp-execline lst)
  (define (sep? sym)
    (and
     (symbol? sym)
     (let* ((v '#(background
		 backtick cd chroot
		 define dollarat elgetopt elgetpositionals
		 elglob emptyenv envfile exec export exportall
		 fdblock fdclose fdmove fdreserve fdswap
		 forbacktickx foreground forstdin forx getcwd getpid gptimage
		 heredoc homeof if ifelse ifte ifthenelse
		 importas loopwhilex multidefine nice pipeline
		 piperw redirfd runblock shift su
		 sudo trap tryexec umask
		 unexport unshare wait withstdinas xargs))
	    (e<? (lambda (a b)
		  (string<? (##sys#symbol->string a) (##sys#symbol->string b))))
	    (ref (lambda (i) (vector-ref v i))))
       (let loop ((i 0)
		  (j (- (vector-length v) 1)))
	 (and (<= i j)
	      (let* ((mid (+ (quotient (- j i) 2) i))
		     (e   (ref mid)))
		(or (eq? e sym)
		    (if (e<? sym e)
			(loop i (- mid 1))
			(loop (+ mid 1) j)))))))))
  (define (execl-dsp obj)
    (cond
      ;; technically there can be spaces, etc. in symbols, too...
      ((symbol? obj)     (display obj))
      ((string? obj)     (%dsp-string obj))
      ((u8vector? obj)   (display (quote-bv obj)))
      ((integer? obj)    (display obj))
      ((real? obj)       (display obj))
      ;; if you write '-i it's read as a complex number;
      ;; this shows up in 'sed -i' for example
      ((complex? obj) (error "you almost certainly didn't mean to print:" obj))
      (else (error "can't serialize for execline:" obj lst))))
  (let loop ((lst    lst) ; items to display
	     (indent 0)
	     (nl     #t)) ; currently in head position 
    (define (cont endl indent)
      (cond
       ((null? endl) (newline))
       ((sep? (car endl))
	(begin
	  (newline)
	  (display (tabs indent))
	  (loop endl indent #t)))
       (else
	(begin
	  (display " ")
	  (loop endl indent #f)))))
    (or (null? lst)
	(let ((head (car lst))
	      (rest (cdr lst)))
	  (cond
	   ((null? head)
	    (begin
	      (when nl
		(error "unexpected null in execline form" lst))
	      (display "{ }")
	      (cont rest indent)))
	   ((list? head)
	    (begin	; display a block
	      (when nl
		;; TODO: this isn't strictly illegal,
		;; but I'm not aware of any tools that
		;; support {{ ... } ...} syntax
		(error "unexpected list in head position" head))
	      ;; if someone writes '(a b ,c) accidentally,
	      ;; bail rather than producing a really strange
	      ;; execline script
	      (when (memq (car head) '(quote unquote unquote-splicing quasiquote))
		(error "not a deliberate execline form" head))
	      (display "{\n")
	      (display (tabs (+ indent 1)))
	      (loop head (+ indent 1) #t)
	      (display (tabs indent))
	      (display "}")
	      (cont rest indent)))
	   (else
	    (begin
	      (execl-dsp head)
	      (cont rest indent))))))))

;; write-exexpr writes an execline expression
;; as a script to current-output-port
;;
;; the "shebang:" keyword argument may specify
;; an alternate script invocation; the default
;; is #!/bin/execlineb -P
(: write-exexpr (list #!rest * -> undefined))
(define (write-exexpr expr #!key (shebang execline-shebang))
  (when shebang
    (begin
      (display shebang)
      (newline)))
  (dsp-execline expr))

(: elif ((or list false) list -> list))
(define (elif head body)
  (unless (pair? body)
    (error "elif: unexpected tail form" body))
  (cond
   ((not head) body)
   ((list? head) (cons 'if (cons head body)))
   (else (error "elif: unexpected head form" head))))

(: elif* ((or list false) #!rest (or list false) -> list))
(define (elif* head . rest)
  (let loop ((head head)
	     (rest rest))
    (if (null? rest)
	head
	(elif head (loop (car rest) (cdr rest))))))

;; el= is a monadic execline template formatter
;; that formats its arguments as a single string
;; where elements are space-separated
(define (el= head . rest)
  (lambda (conf)
    (let ((tail (elexpand conf rest)))
      (string-append
       (cond
	((string? head) head)
	((symbol? head) (##sys#symbol->string head))
	(else (error "unexpected head element in el=" head)))
       (join-with " " tail)))))

;; elconc is a monadic execline template formatter
;; that formats its arguments as a single string
;; by simply concatenating the display'd elements
(define (elconc . args)
  (lambda (conf)
    (let ((lst (elexpand conf args)))
      (with-output-to-string
	(lambda ()
	  (for-each display lst))))))

;; elpath expands a list of arguments into
;; a filepath by concatenating the resolved values
;; into (apply filepath-join args ...)
(define (elpath . args)
  (lambda (conf)
    (apply filepath-join (elexpand conf args))))

(define (elexpand conf lst)
  ;; literal datum (valid in execline form):
  (define (lit? x)
    (or (symbol? x) (string? x) (number? x) (u8vector? x)))

  ;; prepend the k=v forms of kvector 'kv' to lst
  (define (+kv kv lst)
    (kvector-foldl
     kv
     (lambda (k v lst)
       (cons (k=v k v) lst))
     lst))

  (define (->string x)
    (cond
     ((string? x)   x)
     ((symbol? x)   (##sys#symbol->string x))
     ((keyword? x)  (##sys#symbol->string x))
     ((number? x)   (number->string x 10))
     ((u8vector? x) (quote-bv x))
     (else (error "can't stringify" x))))

  ;; k=v displays a key-value pair as "k=v..."
  ;; (always as a single string)
  (define (k=v k v)
    (string-append
      (##sys#symbol->string k)
      "="
      (if (list? v)
	  (join-with " " v)
	  (->string v))))

  (if (null? lst)
      '()
      (let* ((head (car lst))
	     (rest (cdr lst))
	     (tail (elexpand conf rest)))
	(cond
	 ((null? head) tail)
	 ((lit? head)
	  (if (eq? rest tail) lst (cons head tail)))
	 ((kvector? head)
	  (+kv head tail))
	 ((procedure? head)
	  (let ((e (head conf)))
	    (cond
	     ((null? e)    tail)
	     ((lit? e)     (cons e tail))
	     ((pair? e)    (append e tail))
	     ((kvector? e) (+kv e tail))
	     ;; TODO: maybe recurse on procedure?
	     (else         (error "cannot splat" e)))))
	 ((pair? head)
	  (let ((h (elexpand conf head)))
	    (if (and (eq? h head) (eq? rest tail))
		lst
		(cons h tail))))
	 (else (error "unexpected form in execline template" head))))))

(: eltemplate (list -> procedure))
(define (eltemplate form)
  (lambda (conf) (elexpand conf form)))
