(import
  scheme
  (chicken eval)
  (chicken process-context)
  (chicken repl)
  (chicken file)
  (only (chicken string) string-split)
  (srfi 69)
  (only (chicken read-syntax)
	set-parameterized-read-syntax!)
  (only (chicken base) vector-resize)
  (distill filepath)
  (distill eprint))

(import-for-syntax
  (only (chicken string) conc))

;; by default, search dirs are
;; the current directory, followed by
;; the installed prefix + /lib/distill/
(define search-dirs
  (make-parameter
   (list
    (filepath-join
     ;; something like /usr/lib/distill/...
     (executable-pathname) "../lib/distill/"))))

(define (form? head expr)
  (and (pair? expr) (eq? (car expr) head)))

;; we re-parameterize 'eval' so
;; we need to ensure that we're using the right
;; procedure inside load-builtin, etc.
(define real-eval (eval-handler))

;; load-builtin loads a module with the given symbol
;; from (search-dirs)/<kind>/<sym>.scm, taking care to load
;; its dependencies in advance by walking the import table
(define load-builtin
  (let ((loaded (make-hash-table)))
    (lambda (kind sym)
      (or (hash-table-ref/default loaded sym #f)
          (begin
            (hash-table-set! loaded sym #t)
            (let ((file (let loop ((dirs (cons "." (search-dirs))))
                          (if (null? dirs)
                            (error "can't find" kind sym)
                            (let ((f (filepath-join (car dirs) kind (string-append (symbol->string sym) ".scm"))))
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
                  (real-eval `(module (,kind ,sym)
                                (,sym)
                                ,@(read*)))))))))))

(define (scan-imports lst)
  (for-each
    (lambda (im)
      (and-let* ((_ (pair? im))
                 (h (car im))
                 (_ (memq h '(pkg svc plat))))
        (load-builtin h (cadr im))))
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

(define (sum-cmd args)
  (import
    (distill hash))
  (for-each
   (lambda (arg)
     (let ((h (hash-file arg)))
       (if h
	   (begin (display h) (newline))
	   (fatal "file doesn't exist:" arg))))
   (if (null? args)
       (list "/proc/self/fd/0")
       args)))

(define (args->plan args)
  (import
    (distill system))
  (let* ((platname (string->symbol (car args)))
	 (sysfile  (if (null? (cdr args)) "system.scm" (cadr args)))
	 (_        (load-builtin 'plat platname))
	 (plat     (eval `(begin
			    (import (plat ,platname))
			    ,platname)))
	 (_        (%load sysfile))
	 (sys      (eval 'system)))
    (platform+system->plan plat sys)))

(define (build-cmd args)
  (import
    (distill plan))
  (when (null? args)
    (fatal "usage: distill build <platform> [<system> | system.scm]"))
  (let* ((plan     (args->plan args))
	 (art      (begin
		     (build-graph! (list plan))
		     (plan-outputs plan))))
    (info "output is" (artifact-hash art))))

;; list-cmd dumps the dependency graph
;; of the build implied by 'args'
(define (list-cmd args)
  (fatal "list not implemented yet"))

(define (run-cmd args)
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
      ;; when invoked as 'distill run <foo.scm> args ...'
      ;; load the first argument with the remaining args
      ;; as the command-line-arguments
      (parameterize ((program-name           (car args))
		     (command-line-arguments (cdr args))
		     (eval-handler           child-eval))
	(for-each %load args))))

(let ((home (get-environment-variable "HOME"))
      (dirs (get-environment-variable "DISTILL_PATH"))
      (args (command-line-arguments))
      (cmds `((sum   . ,sum-cmd)
	      (build . ,build-cmd)
	      (list  . ,list-cmd)
	      (run   . ,run-cmd))))
  (when dirs
    (search-dirs
     (append (string-split dirs ":") (search-dirs))))
  (if (null? args)
      (begin
	(info "usage: distill <subcommand>")
	(fatal "  subcommands:" (map car cmds)))
      (let* ((sym  (string->symbol (car args)))
	     (cell (assq sym cmds)))
	(if cell
	    ((cdr cell) (cdr args))
	    (fatal "no such command" sym)))))
