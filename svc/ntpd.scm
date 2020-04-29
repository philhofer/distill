(import
  scheme
  (chicken module)
  (distill plan)
  (distill package)
  (distill service)
  (distill fs)
  (distill unix)
  (only (distill sequence) ->lines+spaces)

  (pkg openntpd))

(export default-ntpd-config)

(define default-ntpd-config
  '((servers pool.ntp.org)
    (sensor "*")
    (constraints from "\"https://www.google.com/\"")))

(define (ntpd lines)
  (let* ((confpath "/etc/ntpd.conf")
	 (conf     (interned
		    confpath #o644
		    (->lines+spaces lines))))
    (make-service
     name:   'ntpd
     inputs: (list openntpd conf)
     users:  (list (adduser 'ntpd group: 'ntpd home: "/var/empty"))
     groups: (list (addgroup 'ntpd '(ntpd)))
     after:  (list var-mounted-rw)
     spec:   (longrun*
	      run: `((fdmove -c 2 1)
		     (/usr/sbin/ntpd -d -f ,confpath))))))
