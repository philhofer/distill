(import
  scheme
  (scheme load)
  (chicken process-context)
  (chicken repl)
  (chicken file)
  (srfi 69)
  (only (chicken read-syntax)
	set-parameterized-read-syntax!)
  (only (chicken base) vector-resize)
  (only (chicken string) conc))

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
    memo
    execline
    hash
    filepath
    eprint
    plan
    package
    base))

(import
  (filepath)
  (eprint))

(define search-dirs
  (make-parameter (list ".")))

(define %maybe-load
  (let* ((loaded (make-hash-table))
         (load!  (lambda (relpath)
                   (call/cc
                     (lambda (ret)
                       (for-each
                         (lambda (dir)
                           (let ((full (filepath-join dir relpath)))
                             (when (file-exists? full)
                               (begin (load full) (ret #t)))))
                         (search-dirs))
                       #f)))))
    ;; desc->relpath converts
    ;;  (foo) -> foo.scm
    ;;  (foo bar) -> foo/bar.scm
    ;;  (foo bar baz) -> foo/bar/baz.scm
    ;; ... and so forth
    (define (desc->relpath desc)
      (apply
        filepath-join
        (let loop ((lst desc))
          (if (null? (cdr lst))
            (cons (conc (car lst) ".scm") '())
            (cons (car lst) (loop (cdr lst)))))))
    (lambda (lst)
      (for-each
        (lambda (desc)
          (hash-table-ref loaded desc
                          (lambda ()
                            (or (load! (desc->relpath desc))
                                (fatal "unable to load" desc))
                            (hash-table-set! loaded desc #t))))
        lst))))

;; kind of a hack: expose code-loading syntax
;; through a module available to interpreted code
(eval
  `(module (loader)
     (find-libs)
     (import
       scheme)
     (define-syntax find-libs
       (er-macro-transformer
         (lambda (expr rename cmp)
           (,%maybe-load (cdr expr))
           (let ((_import (rename 'import)))
             (cons _import (cdr expr))))))))

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
