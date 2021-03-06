(import
 scheme
 (chicken file)
 (chicken string)
 (chicken platform)
 (chicken pretty-print))

(define mod-to-import
  ;; mutable, but these ones we know
  ;; in advance:
  '((matchable . "matchable.import.scm")
    ((srfi 69) . "srfi-69.import.scm")))

(define (lib-name form)
  (let ((n (cadr form)))
    (cond
     ((symbol? n) (symbol->string n))
     ((list? n)   (string-intersperse (map symbol->string n) "."))
     (else        (error "unexpected library name" n)))))

(define (lib-import form)
  (string-append (lib-name form) ".import.scm"))

;; perform a left-associative fold
;; over forms beginning with the given symbol
(define (form-fold sym proc seed form)
  (let loop ((lst   (cddr form))
             (state seed))
    (if (null? lst)
        state
        (let ((head (car lst))
              (rest (cdr lst)))
          (if (pair? head)
              (cond
               ((eq? (car head) sym)
                (loop rest (proc state head)))
               ((eq? (car head) 'cond-expand)
                ;; walk each arm of the cond-expand until 'feature?' is #t
                (let inner ((expr (cadr head)))
                  (cond
                   ((null? expr)
                    (error "didn't match any cond-expand expression"))
                   ((eq? (car expr) 'else)
                    (loop rest (loop (cdr expr) state)))
                   ((and (symbol? (car expr)) (feature? (car expr)))
                    (loop rest (loop (cdr expr) state)))
                   ;; TODO: match (and feature ...), (or feature ...)
                   (else (inner (cdr expr))))))
               (else (loop rest state)))
              (loop rest state))))))

;; strip away import modifiers
(define (unpack-import expr)
  (if (and (pair? expr)
           (memq (car expr)
                 '(only except rename prefix)))
      (unpack-import (cadr expr))
      expr))

;; TODO: need to walk cond-expand forms as well
(define (lib-imports form)
  (form-fold
   'import
   (lambda (lst im)
     (foldl
      (lambda (lst iexpr)
        (cons (unpack-import iexpr) lst))
      lst
      (cdr im)))
   '()
   form))

(define (lib-includes form)
  (form-fold
   'include
   (lambda (lst inc)
     (cons (cadr inc) lst))
   '()
   form))

(define (lib-depends form)
  ;; a library depends on locally-defined *.sld
  ;; files and anything that is (include)'d
  (foldl
   (lambda (lst im)
     (let ((v (assoc im mod-to-import)))
       (if v (cons (cdr v) lst) lst)))
   (lib-includes form)
   (lib-imports form)))

;; %.sld -> %.mod.scm
(define (modfile-name orig)
  (string-append
   (substring orig 0 (- (string-length orig)
                        (string-length ".sld")))
   ".mod.scm"))

(define (write-mod form modf)
  (define (chicken-import expr)
    ;; normalize 'expr' to a non-R7RS chicken import
    (if (pair? expr)
        (case (car expr)
          ((srfi) (string->symbol (conc "srfi-" (cadr expr))))
          ((only rename prefix except)
           (cons (car expr) (cons (chicken-import (cadr expr)) (cddr expr))))
          (else expr))
        expr))

  ;; walk top-level forms in a define-library
  ;; and rewrite imports as appropriate
  (define (rewrite-sld-form expr)
    (case (car expr)
      ((export) #f)
      ((import) (cons 'import (map chicken-import (cdr expr))))
      ((cond-expand) (cons 'cond-expand
                           (map (lambda (cpr)
                                  (cons (car cpr) (map rewrite-sld-form (cdr cpr))))
                                (cdr expr))))
      ((include include-ci begin) expr)
      (else expr)))
  (with-output-to-file
      modf
    (lambda ()
      (pp
       `(module ,(cadr form)
                ,(form-fold
                  'export
                  (lambda (lst expr)
                    (append (cdr expr) lst))
                  '()
                  form)
                ,@(let loop ((rest (cddr form)))
                    (if (null? rest)
                        '()
                        (let ((v (rewrite-sld-form (car rest))))
                          (if v (cons v (loop (cdr rest))) (loop (cdr rest)))))))))))

(define slds
  (map
   (lambda (f)
     (let ((form (call-with-input-file f (cut read <>))))
       (unless (eq? (car form) 'define-library)
         (error "expected define-library form in" f))
       (set! mod-to-import
             (cons
              (cons (cadr form) (lib-import form))
              mod-to-import))
       (let ((modfile (modfile-name f)))
         (write-mod form modfile)
         (cons (cons f modfile) form))))
   (glob "*.sld")))

(display "# generated by autodep.scm\n")
(for-each
 (lambda (sld)
   (let* ((form  (cdr sld))
          (filep (car sld))
          (sldf  (car filep))
          (modf  (cdr filep))
          (deps  (lib-depends form))
          (name  (lib-name form)))
     (display modf)
     (display ": ")
     (display sldf)
     (newline)
     (display (string-append name ".import.scm "))
     (display (string-append name ".c: "))
     (display modf)
     (display " ")
     (display (string-intersperse deps " "))
     (newline)))
 slds)
