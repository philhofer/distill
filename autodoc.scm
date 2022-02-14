(import
 (chicken io)
 (chicken port)
 (chicken pretty-print)
 (chicken string)) ;; substring=?

;; see srfi 13
(: string-prefix? (string string --> boolean))
(define (string-prefix? pre str)
  (let ((plen (string-length pre))
        (slen (string-length str)))
    (and (<= plen slen)
         (substring=? pre str 0 0 plen))))

;; see srfi 13
(: string-suffix? (string string --> boolean))
(define (string-suffix? suff str)
  (let ((elen (string-length suff))
        (slen (string-length str)))
    (and (<= elen slen)
         (substring=? str suff (- slen elen) 0 elen))))

(define (filter keep? lst)
  (let loop ((out '())
             (in lst))
    (cond
     ((null? in)
      out)
     ((keep? (car in))
      (loop (cons (car in) out) (cdr in)))
     (else (loop out (cdr in))))))

(define (form? sym lst)
  (and (pair? lst)
       (eq? (car lst) sym)))

(define (extract-exports sld)
  ;; extract each (export ...) form into forms
  (let ((forms (filter
                (cut form? 'export <>)
                (with-input-from-file sld read))))
    ;; extract each symbol in (export ...)
    (let loop ((in  forms)
               (out '()))
      (if (null? in)
          out
          (let ((form (car in))
                (rest (cdr in)))
            (loop rest (append (cdr form) out)))))))

;; toplevel-define extracts
;; a definition from a top-level
;; form, or returns #f if the
;; form is not a define or define-syntax form
;;
;; for example:
;;   (toplevel-define '(define x ...)) -> x
;;   (toplevel-define '(define (x y) ...)) -> x
;;   (toplevel-define '(define-syntax x ...)) -> x
(define (toplevel-define form)
  (and (pair? form)
       (case (car form)
         ((define)
          (if (pair? (cadr form))
              (caadr form)
              (cadr form)))
         ((define-syntax)
          (cadr form))
         (else #f))))

(define (current-line)
  (let-values (((row col) (port-position)))
    row))

(define (next-line) (+ 1 (current-line)))

;; form->stub turns
;;   (define (a b c) (d e f))
;; into
;;   (define (a b c) ...)
;; when the body is 'large'
(define (form->stub form)
  ;; try to capture a form
  ;; with 10 or fewer leaf datums
  (define (small? form)
    (define (sub form budget exit)
      (cond
       ((pair? form)
        ;; don't recur indefinitely;
        ;; if we exhaust the budget recursing,
        ;; then just bail out
        (sub (cdr form) (sub (car form) (- budget 1) exit) exit))
       ((vector? form)
        (- budget (vector-length form)))
       (else (- budget 1))))
    (call/cc
     (lambda (exit)
       (>= (sub form 10 exit) 0))))
  (let ((head  (car form))
        (inner (cadr form)))
    (if (and (pair? (cddr form))
             (small? (caddr form)))
        form
        `(,head ,inner ...))))

;; define-ranges produces a list like
;;   '((form start-line end-line) ...)
;; where the start-line and end-line positions
;; indicate the regions in which a definition
;; and possibly an enclosing comment would live
;;
;; returned forms are in line-sorted-order
(define (define-ranges exports file)
  ;; if we read (: name type ...)
  ;; before (define (name ...) ...),
  ;; then return those together;
  ;; otherwise return the latter and #f
  (define (read-typed)
    (let ((first (read)))
      (if (and (pair? first)
               (eq? (car first) ':))
          (let ((next (read)))
            (values next (if (eq? (toplevel-define next) (cadr first))
                             first
                             #f)))
          (values first #f))))
  (with-input-from-file file
    (lambda ()
      (let loop ((out       '())
                 (prev-line 0))
        (receive (form typeinfo) (read-typed)
          (if (eof-object? form)
              (reverse out)
              (let* ((def  (toplevel-define form))
                     (next (next-line))
                     (out  (if (and def (memq def exports))
                               (cons (list def prev-line next (form->stub form) typeinfo) out)
                               out)))
                (loop out next))))))))

(define (ranges->comments ranges file)
  (define (ff! lineno target)
    (if (< lineno target)
        (and (not (eof-object? (read-line)))
             (ff! (current-line) target))
        #t))
  (define (string-plus prev next)
    (if prev
        (string-append prev next "\n")
        (string-append next "\n")))
  ;; read the comments that occur most recently
  ;; up to the first s-expression;
  ;; the return value is a string or #f
  (define (read-comments)
    (let loop ((out #f))
      (let ((line (read-line)))
        (cond
         ((string-prefix? ";; " line)
          (loop
           (string-plus out (substring line 3))))
         ((string=? line "")
          (loop #f))
         ((string-prefix? "(" line)
          out)
         (else out)))))
  (with-input-from-file file
    (lambda ()
      (let loop ((lineno 0)
                 (ranges ranges)
                 (out    '()))
        (if (null? ranges)
            out
            (let* ((head  (car ranges))
                   (rest  (cdr ranges))
                   (start (cadr head))
                   (end   (caddr head))
                   (more  (cdddr head))
                   (stub  (car more))
                   (type  (cadr more)))
              (ff! lineno start)
              (let ((comment (read-comments)))
                (loop
                 (current-line)
                 rest
                 (if comment (cons (cons (if type (vector stub type) stub) comment) out) out)))))))))

(define (docs name)
  (let* ((sld    (string-append name ".sld"))
         (body   (string-append name ".scm"))
         (ex     (extract-exports sld))
         (ranges (define-ranges ex body)))
    (ranges->comments ranges body)))

(define (modules->markdown lst)
  ;; print documentation for a pair
  (define (print-doc p)
    (let* ((val     (car p))
           (comment (cdr p))
           (form    (if (vector? val)
                        (vector-ref val 0)
                        val))
           (type    (and (vector? val)
                         (vector-ref val 1))))
      (print "### " (toplevel-define form))
      (print "```")
      (if type
          (begin
            (pp type)
            (pp form))
          (pp form))
      (print "```")
      (print (cdr p))))
  (for-each
   (lambda (mod)
     (print "## module `(distill " mod ")`")
     (for-each print-doc (docs mod)))
   lst))

(define (main args)
  (print "# Module Reference")
  (modules->markdown
   '("base"
     "package" "eprint" "fetch"
     "fs" "hash" "image" "net"
     "plan" "text" "tai64" "unix"
     "system" "filepath")))
