(import
 scheme
 (chicken module)
 (distill unix)
 (distill plan)
 (distill text)
 (distill fs)
 (distill service)
 (distill execline)
 (distill package)
 (pkg chrony)
 (pkg ip-wait))

(export default-chronyd-config)
(define default-chronyd-config
  '((pool pool.ntp.org iburst)
    (initstepslew 10 pool.ntp.org)
    (driftfile /var/lib/chrony/chrony.drift)
    (rtcsync)
    (cmdport 0)))

;; don't pull in /var/lib/chrony*
(define chrony-binaries
  (binaries chrony))

(define (chronyd config)
  (let ((conf (interned "/etc/chrony.conf" #o644 (lines config))))
    (make-service
     name: 'chronyd
     inputs: (list chrony-binaries conf ip-wait)
     users:  (list (adduser 'chrony group: 'chrony home: "/var/empty"))
     groups: (list (addgroup 'chrony '(chrony)))
     after:  (list var-mounted-rw)
     spec:   (longrun*
              run: (elif*
                    '(mkdir -p /var/lib/chrony)
                    '(ip-wait route "^default") ; wait for networking
                    '(fdmove -c 2 1 chronyd -d -u chrony -F 2))))))
