(import
  scheme
  (distill text)
  (distill service)
  (distill execline)
  (only (distill filepath) filepath-join)
  (only (distill unix) adduser addgroup)
  (only (distill plan) interned)
  (distill fs)
  (pkg dhcpcd))


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

(define (auto-dhcpcd lst)
  ;; TODO:
  ;;  - use notification-fd and hooks
  ;;    to signal readiness when a default route
  ;;    is installed (or figure out another readiness scheme)
  ;; - use hooks to set /etc/resolv.conf or equivalent
  ;; - bind less stuff into the chroot (less attack surface)
  (let* ((chroot-bind '(proc sys dev))
	 ;; dhcpcd puts the lease db into the chroot,
	 ;; so it ought to be in persistent r+w storage
	 (chroot-dir  "/var/chroot/dhcpcd") 
	 (chroot-fmt  (lambda (mnt)
			(filepath-join chroot-dir mnt))))
    (make-service
     name:   'net.dhcpcd
     inputs: (list dhcpcd
		   hook-script
		   (interned "/etc/dhcpcd.conf" #o644 (lines lst)))
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
