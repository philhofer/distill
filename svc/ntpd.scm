(import
  scheme
  (chicken module)
  (distill plan)
  (distill package)
  (distill service)
  (distill fs)
  (distill unix)
  (distill text)

  (pkg ip-wait)
  (pkg openntpd))

(export default-ntpd-config)

(define default-ntpd-config
  '((servers pool.ntp.org)
    (sensor "*")
    (constraints from "\"https://www.google.com/\"")))

(define (ntpd cfg)
  (let* ((confpath "/etc/ntpd.conf")
	 (conf     (interned
		    confpath #o644
		    (lines cfg))))
    (make-service
     name:   'ntpd
     inputs: (list openntpd ip-wait conf)
     users:  (list (adduser 'ntpd group: 'ntpd home: "/var/empty"))
     groups: (list (addgroup 'ntpd '(ntpd)))
     after:  (list var-mounted-rw)
     spec:   (longrun*
	      run: `(fdmove -c 2 1
			    ;; no use starting until we have a default route:
			    if (s6-setuidgid nobody ip-wait route "^default")
			    /usr/sbin/ntpd -s -d -f ,confpath)))))
