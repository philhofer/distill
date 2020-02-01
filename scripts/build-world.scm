(import
  scheme
  (distill plan)
  (distill filepath)
  (distill buildenv)
  (chicken file)
  (chicken process-context)
  (distill package)
  (only (srfi 1) filter)
  (only (srfi 13) substring/shared))

;; this script builds every package in pkg/*.scm
;; (if it is out-of-date)

(define (package-handle name)
  (eval `(import (pkg ,name)))
  (eval name))

(define (trim-suffix str suff)
  (let* ((strlen (string-length str))
         (suflen (string-length suff))
         (diff   (- strlen suflen)))
    (and (>= diff 0)
         (let ((end (substring/shared str diff strlen)))
           (and (string=? end suff)
                (substring/shared str 0 diff))))))

(define package-names
  (find-files
    "./pkg"
    action: (lambda (f lst)
              (let ((suf (trim-suffix f ".scm")))
                (if suf (cons (string->symbol (basename suf)) lst) lst)))
    seed: '()))

;; filter-for-config filters out packages
;; that are known not to cross-compile
(define (filter-for-config conf lst)
  (if (eq? (conf 'arch) *this-machine*)
    lst
    (let ((no-cross '(perl)))
      (filter
        (lambda (p)
          (not (memq p no-cross)))
        lst))))

(let* ((config  (let ((args (command-line-arguments)))
                  (if (null? args)
                    (build-config)
                    (default-config (string->symbol (car args))))))
       (build!  (config->builder config))
       (names   (filter-for-config config package-names))
       (handles (map package-handle names))
       (outputs (apply build! handles)))
  (for-each
    (lambda (p)
      (display (car p))
      (display " ")
      (display (short-hash (artifact-hash (cdr p))))
      (newline))
    (map cons names outputs)))
