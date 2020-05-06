(import
  scheme
  (distill plan)
  (distill execline)
  (distill unix)
  (distill service)
  (distill fs)
  (distill text)
  (pkg openssh))

;; sshd runs the sshd service
;; using the configuration from
;; $PWD/etc/sshd_config
(define (sshd config-lines)
  (let* ((confpath "/etc/sshd_config")
	 (config   (interned
		    confpath #o644
		    (lines config-lines))))
    (make-service
     name:   'sshd
     users:  (list (adduser 'sshd group: 'sshd home: "/var/empty"))
     groups: (list (addgroup 'sshd '(sshd)))
     inputs: (list openssh config)
     after:  (list var-mounted-rw)
     ;; TODO: after: if we are not doing
     ;; a wildcard bind, then we must wait
     ;; for interfaces to have the addresses
     ;; to which sshd will bind...
     spec:   (longrun*
	      run: `((fdmove -c 2 1)
		     (if ((mkdir -p /var/empty)))
		     (if ((if -t -n ((test -f /var/etc/ssh/ssh_host_ed25519_key)))
			  (if ((mkdir -p /var/etc/ssh)))
			  ;; NOTE: using ssh-keygen -A here doesn't work,
			  ;; because it disagrees with sshd on where the
			  ;; keys ought to be found relative to the install prefix
			  (foreground ((echo "generating new host key")))
			  (ssh-keygen -t ed25519 -P "" -f /var/etc/ssh/ssh_host_ed25519_key)))
		     ;; sshd needs to be invoked with an absolute path
		     ;; in order for privsep re-exec to work
		     (/usr/sbin/sshd -h /var/etc/ssh/ssh_host_ed25519_key
				     -D -f ,confpath))))))
