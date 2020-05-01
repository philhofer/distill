(import
  scheme
  (only (chicken base) error)
  (distill filepath)
  (distill base)
  (distill plan)
  (distill sequence)
  (distill service))

;; busybox crond
;;
;; cron accepts an alist of the form
;; '((user . ((line ...)
;;            ...))
;;    ...)
;; e.g.
;; '((root ("*/15" * * * * run-me-quarter-hourly ...)
;;         (0      * * * * run-me-hourly ...)
;;         (0      0 * * * run-at-midnight ...)))
;; and produces a service that runs crond
;; with crontabs set to the given user directories
;; and crontab entries
(define (cron alist #!key (dir "/etc/crontabs"))
  (let* ((cron-line? (lambda (x)
		      (if (and (list? x)
			       (>= (length x) 6))
			  x
			  (error "not a valid crontab line" x))))
	 (cron-lines (lambda (lst)
		       (for-each cron-line? lst) lst))
	 (inputs     (map
		      (lambda (p)
			(interned
			 (filepath-join dir (car p))
			 #o644
			 (->lines+spaces (cron-lines (cdr p)))))
		      alist)))
    (make-service
     name:   'cron
     inputs: inputs ;; note: busybox crond from busybox-full is implied
     spec:   (longrun*
	      run: `((fdmove -c 2 1)
		     (crond -f -d 8 -c ,dir))))))
