(import
  scheme
  (distill fs)
  (distill service))

;; urandom-seed saves some state from
;; /dev/urandom and restores it
;; on the next boot
(define urandom-seed
  (make-service
   name: 'urandom-seed
   after: (list var-mounted-rw)
   spec: (oneshot*
	  up: '((foreground ((if ((test -O /var/urandom-seed)))
			     (redirfd -w 1 /dev/urandom)
			     (cat /var/urandom-seed)))
		(true))
	  down: '((foreground ((rm -f /var/urandom-seed)))
		  (umask 077)
		  (if ((touch /var/urandom-seed)))
		  (dd if=/dev/urandom of=/var/urandom-seed bs=1K count=1)))))
