;; write-setparts-script writes an execlineb script
;; that takes the destination device as the first argument
;; and dd's each given artifact to the device's partitions
;; in sequence
;; (#f indicates the partition should be skipped)
(define (write-setparts-script lst)
  (let ((exprs (let loop ((n 1)
			  (in lst))
		 (cond
		  ((null? in) '())
		  ((not (car in)) (loop (+ n 1) (cdr in)))
		  (else
		   (cons
		    (list (string-append "${1}p" (number->string n 10))
			  (abspath (artifact-path (car in))))
		    (loop (+ n 1) (cdr in))))))))
    (write-exexpr
     `((setparts (,@exprs))
       ("$@"))
     shebang: "#!/bin/execlineb -s1")))

(define-kvector-type
  <system>
  %make-system
  system?
  (system-services services: '() (list-of service?))
  (system-packages packages: '() list?))

;; build-system takes a platform and a set of keyword+value arguments
;; and runs the platform's build function
;;
;; recognized keywords are:
;;   services: a list of system services (required)
;;   packages: an additional list of packages to be installed (optional)
;;
(define build-system
  (letrec ((union/eq?  (lambda (a b)
			 (cond
			  ((null? a) b)
			  ((null? b) a)
			  (else
			   (let ((fb (car b)))
			     (union/eq?
			      (if (memq fb a) a (cons fb a))
			      (cdr b))))))))
    (lambda (plat . args)
      (let ((sys  (apply %make-system args)))
	;; TODO: this is only doing basic deduplication
	;; of packages; we should probably figure out
	;; how to generate friendlier errors when
	;; packages conflict (overlap)
	(plat (union/eq? (services->packages (system-services sys))
			 (system-packages sys)))))))

;; uniq-setparts-script produces a uniquely-named setparts script
(define (uniq-setparts-script prefix parts)
  ;; artifacts that become raw disk partitions
  ;; should be raw files, not archives, symlinks, etc.
  (for-each
    (lambda (art)
      (unless (eq? (artifact-kind art) 'file)
        (info "warning: artifact" (short-hash (artifact-hash art)) "is of kind" (artifact-kind art))))
    parts)
  (let* ((h (apply string-append (map artifact-hash parts)))
         (f (string-append prefix (substring h 0 10))))
    (info "writing final privileged script to" f)
    (with-output-to-file
      f
      (lambda () (write-setparts-script parts)))
    (set-file-permissions! f #o755)))
