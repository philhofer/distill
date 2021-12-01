(import
 scheme
 (srfi 69)
 (chicken eval)
 (chicken process-context)
 (chicken repl)
 (chicken file)
 (only (chicken string) string-split)
 (only (chicken condition) print-error-message)
 (only (chicken io) read-list write-string)
 (only (chicken port) with-input-from-string)

 (only (chicken read-syntax)
       set-parameterized-read-syntax!)
 (only (chicken base) vector-resize print)
 (only (distill coroutine)
       push-exception-wrapper)
 (distill filepath)
 (distill eprint))

(import-for-syntax
 (only (chicken string) conc))

;; we need these to be loaded,
;; but we don't need them present
;; in the binding environment
(let ()
  (import
   (distill service)
   (distill fs)
   (distill image)
   (distill system)
   (distill net)))

;; by default, search dirs are
;; the current directory, followed by
;; the installed prefix + /lib/distill/
(define search-dirs
  (make-parameter
   (list
    (filepath-join
     ;; something like /usr/lib/distill/...
     (executable-pathname) "../lib/distill/"))))

(let ((envparam (lambda (p e)
                  (and-let* ((v (get-environment-variable e)))
                            (p v)))))
  (import (distill plan))
  (envparam artifact-dir "DISTILL_ARTIFACT_DIR")
  (envparam plan-dir "DISTILL_PLAN_DIR"))

(define (form? head expr)
  (and (pair? expr) (eq? (car expr) head)))

;; we re-parameterize 'eval' so
;; we need to ensure that we're using the right
;; procedure inside load-builtin, etc.
(define real-eval (eval-handler))

;; intern all of the modules in pkg/, plat/, etc.
;; so that they can be hot-loaded
(begin-for-syntax
 (import
  (chicken file)
  (only (chicken io) read-string)
  (chicken sort))
 (define (read-file name)
   (with-input-from-file name read-string))
 (define interned-files
   (let* ((dirs     '(pkg svc plat))
          (dir-files (lambda (dir)
                       (sort
                        (glob (string-append (symbol->string dir) "/*.scm"))
                        string<?)))
          (files     (map dir-files dirs)))
     (foldl
      (lambda (alist fileset)
        (foldl
         (lambda (alist file)
           (cons (cons file (read-file file)) alist))
         alist
         fileset))
      '()
      files))))

(define-syntax loaded-files
  (er-macro-transformer
   (lambda (exp rename compare)
     `(quote ,interned-files))))

(define *preload-alist* (loaded-files))

;; load-builtin loads a module with the given symbol
;; from (search-dirs)/<kind>/<sym>.scm, taking care to load
;; its dependencies in advance by walking the import table
;;
;; if the file is not present in one of those directories
;; but it *is* present in the preloaded file alist,
;; then that file is used instead
(define load-builtin
  (let ((loaded (make-hash-table)))
    (lambda (kind sym)
      (or (hash-table-ref/default loaded sym #f)
          (begin
            (hash-table-set! loaded sym #t)
            (let* ((fullname (filepath-join kind (string-append (symbol->string sym) ".scm")))
                   (file     (let loop ((dirs (cons "." (search-dirs))))
                               (if (null? dirs)
                                   #f
                                   (let ((f (filepath-join (car dirs) fullname)))
                                     (if (file-exists? f) f (loop (cdr dirs)))))))
                   (with-input (lambda (thunk)
                                 (if file
                                     (with-input-from-file file thunk)
                                     (let ((pr (assoc fullname *preload-alist*)))
                                       (and pr (with-input-from-string (cdr pr) thunk))))))
                   (forms      (with-input read-list)))
              (if forms
                  (begin
                    (for-each
                     (lambda (form)
                       (when (form? 'import form)
                         (scan-imports form)))
                     forms)
                    (real-eval
                     `(module (,kind ,sym)
                              (,sym)
                              ,@forms)))
                  (fatal "couldn't locate" fullname))))))))

(define (scan-imports lst)
  (for-each
   (lambda (im)
     (and-let* ((_ (pair? im))
                (h (car im))
                (_ (memq h '(pkg svc plat))))
               (push-exception-wrapper
                (lambda (exn)
                  (print-error-message exn)
                  (info "couldn't load" im exn)
                  exn)
                (lambda ()
                  (load-builtin h (cadr im))))))
   lst))

(define (%load file)
  (with-input-from-file file
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

(define (builtin-cmd args)
  (for-each
   (lambda (pr)
     (print (car pr)))
   *preload-alist*))

(define (write-file name datums)
  (with-output-to-file name
    (lambda ()
      (for-each write datums))))

(define (edit-cmd args)
  (for-each
   (lambda (name)
     (let ((pr (assoc name *preload-alist*)))
       (if pr
           (with-output-to-file name
             (lambda ()
               (write-string (cdr pr))))
           (fatal name "not a builtin file"))))
   args))

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

(define (again-cmd args)
  (import
   (distill plan))
  (when (null? args)
    (fatal "usage: distill again <plan-hash> ..."))
  (for-each build-plan! (map load-plan args)))

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
    (display (artifact-hash art))
    (newline)))

(define (intern-cmd args)
  (import
   (distill plan)
   (distill hash)
   (only (chicken file) rename-file))
  (for-each
   (lambda (arg)
     (let* ((f (if (string=? arg "-")
                   (current-input-port)
                   (open-input-file arg)))
            (dir (artifact-dir))
            (tmp (filepath-join dir (string-append (basename arg) ".tmp")))
            (t (open-output-file tmp))
            (h (copy-port+hash f t)))
       (close-input-port f)
       (close-output-port t)
       (print h)
       (rename-file
        tmp (filepath-join dir h))))
   (if (null? args) '("-") args)))

;; list-cmd dumps the dependency graph
;; of the build implied by 'args'
(define (list-cmd args)
  (import
   matchable
   (distill plan))
  (define (artifact-url x)
    (match (artifact-format x)
           (#('archive kind)
            (artifact-extra x))
           (#('file path mode)
            (let ((extra (artifact-extra x)))
              (and (pair? extra)
                   (memq (car extra) '(remote local))
                   (cdr extra))))
           (else #f)))
  (define (print-artifact x)
    (apply
     print
     (artifact-hash x)
     " "
     (match (artifact-format x)
            (`#(file ,path ,mode)
             (list
              "file "
              path
              " "
              (number->string mode 8)
              " "
              (match (artifact-extra x)
                     (`(inline . ,str) "<inline>")
                     (`(remote . ,url) url)
                     (`(local . ,path) path)
                     ('#f "<autogenerated>")
                     (else "???"))))
            (`#(archive ,fmt)
             (list
              "archive "
              (or (artifact-extra x)
                  "<local>")))
            (`#(symlink ,from ,to)
             (list "symlink " from " -> " to))
            (`#(dir ,path ,mode)
             (list "dir " path " " (number->string mode 8)))
            (else (list "???")))))
  (match args
         (("anchors" . rest)
          (for-each-anchor
           print-artifact
           (list (args->plan rest))))
         (("sources" . rest)
          (for-each-anchor
           (lambda (x)
             (and-let* ((url (artifact-url x)))
                       (print (artifact-hash x) " " url)))
           (list (args->plan rest))))
         (else
          (begin
            (info "list [ anchors | sources ] <plat> [ <system> ]")
            (fatal "unrecognized (show ...) form")))))

(define (run-cmd args)
  (define (child-eval form . rest)
    (when (form? 'import form) (scan-imports (cdr form)))
    (apply real-eval form rest))
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

(define (init-cmd args)
  (import (chicken file))
  (let ((need-dirs '("pkg" "svc" "plat" "artifacts" "plans")))
    (for-each create-directory need-dirs)
    (with-output-to-file "system.scm"
      (lambda ()
        ;; write a skeleton system.scm file
        (write
         '(import
           scheme
           (distill base)
           (distill system)
           (define system
             (make-system
              services: '()
              packages: '()))))))))

;; distill gc <plat> <system.scm>
;; garbage collects everything that isn't
;; involved in building the specified
;; filesystem image
(define (gc-cmd args)
  (import
   (distill plan))
  (when (null? args)
    (fatal "usage: distill gc <plat> <system.scm>"))
  (let* ((plan (args->plan args))
         (live (live-artifact-hashes (list plan))))
    (find-files
     (artifact-dir)
     test: (lambda (f)
             (not (hash-table-ref/default live (basename f) #f)))
     action: (lambda (f lst)
               (print "removing " f)
               (delete-file* f)))))

(let ((dirs (get-environment-variable "DISTILL_PATH"))
      (args (command-line-arguments))
      (cmds `((sum    . ,sum-cmd)
              (build  . ,build-cmd)
              (gc     . ,gc-cmd)
              (list   . ,list-cmd)
              (run    . ,run-cmd)
              (intern . ,intern-cmd)
              (again  . ,again-cmd)
              (builtin . ,builtin-cmd)
              (edit    . ,edit-cmd)
              (init    . ,init-cmd))))
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
