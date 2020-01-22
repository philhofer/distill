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

;; escaper yields the printing procedure for a string
(: escaper (string --> undefined))
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
      (else (error "can't seralize for execline:" obj))))
  (define (join-cmds lst indent)
    (for-each
      (lambda (cmd)
        (display (tabs indent))
        (join-arg (car cmd) (cdr cmd) indent)
        (newline))
      lst))
  (define (join-arg arg rest indent)
    (if (list? arg)
      (begin
        (display "{\n")
        (join-cmds arg (+ indent 1))
        (display (tabs indent))
        (display "}"))
      (execl-dsp arg))
    (unless (null? rest)
      (display " ")
      (join-arg (car rest) (cdr rest) indent)))
  (join-cmds lst 0))

(define (foldl1 proc init lst)
  (let loop ((val init)
             (lst lst))
    (if (null? lst)
      val
      (loop (proc val (car lst)) (cdr lst)))))

;; write-exexpr writes an execline expression
;; as a script to current-output-port
(: write-exexpr (list -> undefined))
(define (write-exexpr expr)
  (display execline-shebang)
  (display "\n")
  (dsp-execline expr))

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

;; check-terminal checks the structure of an execline program
;; and determines whether or not each block consists of zero
;; or more non-terminal statements and finishes with one terminal
;; statement
;;
;; it will either return #t or error
(define (check-terminal lst)
  (define (any p? lst)
    (and (not (null? lst))
         (or (p? (car lst))
             (any p? (cdr lst)))))
  (define (all p? lst)
    (or (null? lst)
        (and (p? (car lst))
             (all p? (cdr lst)))))

  (define (tailn n lst)
    (let ((len (length lst)))
      (let loop ((i   len)
                 (lst lst))
        (if (or (= i n) (null? lst))
          lst
          (loop (- i 1) (cdr lst))))))

  (define (nonterminal-cmd? lst)
    (define (tailcheck n exe)
      (let ((tail (tailn n (cdr lst))))
        (unless (= (length tail) n)
          (error "wrong number of trailing arguments in command:" lst))
        (unless (all list? tail)
          (error "expected trailing blocks in command:" lst))
        (unless (or (not exe)
                    (all check-terminal tail))
          (error "block isn't terminal:" lst))))
    (case (car lst)
      ((if background foreground backtick
         forbacktickx trap tryexec pipeline)
       (begin
         (tailcheck 1 #t) #t))
      ((ifelse ifte)
       (begin
         (tailcheck 2 #t) #t))
      ((ifthenelse)
       (begin
         (tailcheck 3 #t) #t))
      ((multidefine forx wait)
       (begin
         (tailcheck 1 #f) #t))
      ((cd define dollarat elgetopt elgetpositionals
           elglob emptyenv envfile exec export fdblock
           fdclose fdmove fdreserve fdswap forstdin
           getcwd getpid heredoc homeof importas
           piperw redirfd runblock shift umask
           unexport withstdinas chroot unshare
           loopwhilex xargs sudo su nice)
       ;; none of these should have block arguments
       (begin
         (when (any list? (cdr lst))
           (error "bad non-terminal command syntax" lst))
         #t))
      (else #f)))

  (let loop ((head (car lst))
             (rest (cdr lst)))
    (if (null? rest)
      (begin
        (when (nonterminal-cmd? head)
          (error "expected terminal command:" head))
        #t)
      (begin
        (unless (nonterminal-cmd? head)
          (error "expected nonterminal command:" head))
        (loop (car rest) (cdr rest))))))

