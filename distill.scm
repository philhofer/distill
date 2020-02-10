(import
  scheme
  (scheme read)
  (scheme load)
  (chicken eval)
  (chicken process-context)
  (chicken repl)
  (chicken file)
  (srfi 69)
  (only (chicken read-syntax)
	set-parameterized-read-syntax!)
  (only (chicken base) vector-resize))

(import-for-syntax
  (scheme read)
  (only (chicken string) conc))

;; this registers the following units as compiled modules
;; (and declares that they should be linked in) without
;; pulling them into the top-level environment
(let-syntax ((include-imports
               (er-macro-transformer
                 (lambda (expr rename cmp)
                   (cons 'begin
                         (cons
                           `(declare (uses ,@(cdr expr)))
                           (map
                             ;; the 'foo.import.scm' files generally contain two expressions;
                             ;; the first is (eval '(import-syntax ...)) in case syntax definitions
                             ;; reference imported symbols, and the second is (##sys#register-compiled-module ...);
                             ;; we're only interested in the second one because we can just import-syntax here
                             (lambda (sym)
                               (with-input-from-file
                                 (conc sym ".import.scm")
                                 (lambda ()
                                   (let loop ((expr (read)))
                                     (cond
                                       ((eof-object? expr)
                                        (error "didn't find import module for" sym))
                                       ((eq? (car expr) '##sys#register-compiled-module)
                                        expr)
                                       (else
                                         (loop (read))))))))
                             (cdr expr))))))))
  (include-imports
    distill.memo
    distill.nproc
    distill.execline
    distill.hash
    distill.filepath
    distill.sequence
    distill.eprint
    distill.plan
    distill.package
    distill.buildenv
    distill.image
    distill.base
    distill.linux
    distill.service))

(import
  (distill filepath)
  (distill eprint))

(define search-dirs
  (make-parameter (list ".")))

(define (form? head expr)
  (and (pair? expr) (eq? (car expr) head)))

;; we re-parameterize 'eval' so
;; we need to ensure that we're using the right
;; procedure inside load-package, etc.
(define real-eval (eval-handler))

;; load-package loads a package with the given symbol
;; from (search-dirs)/<sym>.scm, taking care to load
;; its dependencies in advance by walking the import table
(define load-package
  (let ((loaded (make-hash-table)))
    (lambda (sym)
      (or (hash-table-ref/default loaded sym #f)
          (begin
            (hash-table-set! loaded sym #t)
            (let ((file (let loop ((dirs (search-dirs)))
                          (if (null? dirs)
                            (error "can't find pkg" sym)
                            (let ((f (filepath-join (car dirs) "pkg" (string-append (symbol->string sym) ".scm"))))
                              (if (file-exists? f) f (loop (cdr dirs)))))))
                  (read* (lambda ()
                           (let loop ((datum (read)))
                             (if (eof-object? datum)
                               '()
                               (begin
                                 (when (form? 'import datum)
                                   (scan-imports (cdr datum)))
                                 (cons datum (loop (read)))))))))
              (with-input-from-file
                file
                (lambda ()
                  (trace "load pkg:" file)
                  (real-eval `(module (pkg ,sym)
                                (,sym)
                                ,@(read*)))))))))))

(define (scan-imports lst)
  (for-each
    (lambda (im)
      (when (and (pair? im) (eq? (car im) 'pkg))
        (load-package (cadr im))))
    lst))

(define (%load file)
  (with-input-from-file
    file
    (lambda ()
      (let loop ((expr (read)))
        (or (eof-object? expr)
            (begin
              (when (form? 'import expr)
                (scan-imports (cdr expr)))
              (and-let* ((_  (form? 'module expr))
                         (im (cadddr expr))
                         (_  (form? 'import im)))
                (scan-imports (cdr im)))
              (real-eval expr)
              (loop (read))))))))

(define (child-eval form . rest)
  (when (form? 'import form) (scan-imports (cdr form)))
  (apply real-eval form rest))

(let ((args (command-line-arguments)))
  (if (null? args)
    (let* ((nv    0)
	   (sav   (vector #f #f #f #f))
	   (push! (lambda (v)
		    (when (not (eq? v (void)))
		      (when (>= nv (vector-length sav))
			(let ((newn (* nv 2)))
			  (set! sav (vector-resize sav newn #f))))
		      (vector-set! sav nv v)
		      (set! nv (+ nv 1)))))
	   (ref   (lambda (n)
		    (if (< n nv)
		      (vector-ref sav n)
		      (void))))
	   (%eval (lambda (expr)
                    (when (form? 'import expr) (scan-imports (cdr expr)))
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

    ;; when invoked as 'distill <foo.scm> args ...'
    ;; load the first argument with the remaining args
    ;; as the command-line-arguments
    (parameterize ((program-name           (car args))
                   (command-line-arguments (cdr args))
                   (eval-handler           child-eval))
      (%load (car args)))))
