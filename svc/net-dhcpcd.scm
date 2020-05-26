(import
  scheme
  (chicken module)
  (only (distill memo) cons*)
  (distill text)
  (distill service)
  (distill execline)
  (only (distill filepath) filepath-join)
  (only (distill unix) adduser addgroup)
  (only (distill plan) interned interned-symlink)
  (distill fs)
  (pkg dhcpcd))

(export
 hook-for-reason
 default-dhcpcd-config)

(define hook-script-path "/usr/libexec/dhcpcd/distill-run-hooks")
(define hook-dir "/etc/dhcpcd/hooks")

;; custom execline-based hook script
(define hook-script
  (interned
   hook-script-path
   #o555
   (lambda ()
     (write-exexpr
      `((importas "-i" reason reason)
	(elglob "-0" -s hooks ,(filepath-join hook-dir "${reason}/*"))
	(forx hook (("${hooks}")))
	(importas "-i" -u hook hook)
	(if ((test -x "${hook}")))
	(foreground ((echo "running hook ${hook}")))
	("${hook}"))))))

(define (hook-for-reason reason name contents)
  (interned
   (filepath-join "/etc/dhcpcd/hooks/" reason name)
   #o555
   contents))

;; have dhcp+dhcp6 nameservers for each interface
;; show up in /run/resolv.<ifname>.dhcp<version>.conf
;; and automatically cat all of those into a new
;; /run/resolv.conf, which is what the /etc/resolv.conf
;; symlink ought to point to
(define resolvconf-hooks
  ;; TODO: handle more than just 'nameserver' entries
  (let* ((nshook (lambda (ipv evar)
		   `((multisubstitute ((importas -Cnu nameservers ,evar)
				       (importas -u |-i| ifname interface)
				       (define version ,ipv)))
		     (mkdir -p /run/resolv.conf.d)
		     (if ((redirfd -w 1 "/run/resolv.${ifname}.dhcp${version}.conf")
			  (forx ns (( $nameservers )))
			  (importas -u |-i| ns ns)
			  (echo nameserver $ns)))
		     (/usr/libexec/dhcpcd/update-resolv-conf))))
	 (upd    '((if ((elglob -0 -s runfiles /run/resolv.*.conf)
			(redirfd -w 1 /run/resolv.conf.new)
			(if -n -t ((test -z $runfiles)))
			(cat $runfiles)))
		   (mv /run/resolv.conf.new /run/resolv.conf)))
	 (v4p     "/usr/libexec/dhcpcd/dhcp-nameserver-hook")
	 (v6p     "/usr/libexec/dhcpcd/dhcp6-nameserver-hook")
	 (v4r     '("BOUND" "RENEW" "REBIND" "REBOOT" "INFORM" "EXPIRE" "STOP" "NAK"))
	 (v6r     '("BOUND6" "RENEW6" "REBIND6" "REBOOT6" "INFORM6" "EXPIRE6" "STOP6" "NAK"))
	 (v4links (map
		   (lambda (reason)
		     (interned-symlink
		      (filepath-join hook-dir reason "v4-resolvconf")
		      v4p))
		   v4r))
	 (v6links (map
		   (lambda (reason)
		     (interned-symlink
		      (filepath-join hook-dir reason "v6-resolvconf")
		      v6p))
		   v6r)))
    (cons*
     (interned-symlink "/etc/resolv.conf" "/run/resolv.conf")
     (interned "/usr/libexec/dhcpcd/update-resolv-conf" #o555
	       (lambda () (write-exexpr upd)))
     (interned v4p #o555
	       (lambda ()
		 (write-exexpr (nshook "4" "new_domain_name_servers"))))
     (interned v6p #o555
	       (lambda ()
		 (write-exexpr (nshook "6" "new_dhcp6_name_servers"))))
     (append v4links v6links))))

(define default-dhcpcd-config
  '((allowinterfaces "eth*")
    (slaac private)
    (option domain_name_servers)))

(define (need-resolvconf-hooks? conf)
  (let loop ((lst conf))
    (and (not (null? lst))
	 (let ((head (car lst))
	       (rest (cdr lst)))
	   (or (and (eq? 'option (car head))
		    (eq? 'domain_name_servers (cadr head)))
	       (loop rest))))))

;; net-dhcpcd creates the net.dhcpcd service
;; with the given config
;;
;; if config contains a (option domain_name_servers) line,
;; then the service will include hooks that manage /etc/resolv.conf
;; using the configuration provided by the DHCP(6) servers
(define (net-dhcpcd config)
  ;; TODO:
  ;; - bind less stuff into the chroot (less attack surface)
  (let* ((chroot-bind '(proc sys dev))
	 ;; dhcpcd puts the lease db into the chroot,
	 ;; so it ought to be in persistent r+w storage
	 (chroot-dir  "/var/chroot/dhcpcd")
	 (chroot-fmt  (lambda (mnt)
			(filepath-join chroot-dir mnt))))
    (make-service
     name:   'net.dhcpcd
     inputs: (cons*
	      dhcpcd
	      hook-script
	      (interned "/etc/dhcpcd.conf" #o644 (lines config))
	      (if (need-resolvconf-hooks? config)
		  resolvconf-hooks
		  '()))
     users:  (list (adduser 'dhcpcd group: 'dhcpcd home: chroot-dir)) ; see '--with-privsep-user'
     groups: (list (addgroup 'dhcpcd '(dhcpcd)))
     after:  (list var-mounted-rw)
     spec:   (longrun*
	      run: `((fdmove -c 2 1)
		     ;; set up chroot bind-mounts:
		     (if ((forx mnt (,chroot-bind))
			  (importas -u "-i" mnt mnt)
			  (define dst ,(chroot-fmt "${mnt}"))
			  (if -n -t ((mountpoint -q "${dst}")))
			  (if ((mkdir -p "${dst}")))
			  (mount --bind "/${mnt}" "${dst}")))
		     (if ((mkdir -p /var/run/dhcpcd)))
		     (dhcpcd -B -c ,hook-script-path -f /etc/dhcpcd.conf))
	      finish: `((forx mnt (,chroot-bind))
			(importas -u "-i" mnt mnt)
			(define dst ,(chroot-fmt "${mnt}"))
			(if ((mountpoint -q "${dst}")))
			(umount "${dst}"))))))
