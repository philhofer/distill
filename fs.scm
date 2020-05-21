;; swap is a service that uses 'dev' for swap
(define (swapon dev)
  (make-service
    name:   (string->symbol (string-append "swap." dev))
    inputs: '()
    after:  (list dev)
    spec:   (oneshot*
              up:   `(;; TODO: detect swap and don't mkswap
                      (fdmove -c 2 1)
                      (foreground ((mkswap ,dev)))
                      (swapon ,dev))
              down: `((fdmove -c 2 1)
                      (foreground ((swapoff ,dev)))
                      (true)))))

;; logging services will generally depend on var-mounted-rw
(define var-mounted-rw 'var-mount)

;; var-mount creates a read-write mount at /var
;;
;; note that the var-mount service is a requirement
;; for persistent logs when the system has a read-only rootfs
;;
;; TODO: mounting /var ends up being in the critical path
;; for many services, like dhcpcd and sshd, so a busted
;; disk would make remote debugging really tough...
;; perhaps we should mount /var as a tmpfs as a last resort?
(define (var-mount dev)
  (let ((opts   '(rw nosuid nodev noexec noatime data=ordered))
        (mkopts '()))
    (make-service
     name:   var-mounted-rw
     inputs: (list e2fsprogs)
     after:  (list dev)
     spec:   (oneshot*
	      up: `((fdmove -c 2 1)
		    (if ((test -b ,dev)))
		    ;; TODO: figure out a better way
		    ;; to determine if this device has actually
		    ;; been formatted yet...
		    (if ((if -t -n ((fsck.ext4 -p ,dev)))
			 (foreground ((echo "fsck didn't work; running mkfs.ext4 on /var ...")))
			 (mkfs.ext4 ,@mkopts ,dev)))
		    (if ((mount -t ext4 -o ,(join-with "," opts) ,dev /var)))
		    (if ((mkdir -p /var/empty /var/db)))
		    (if -t -n ((test -L /var/run)))
		    (foreground ((rm -rf /var/run)))
		    (foreground ((ln -Tnsf /run /var/run)))
		    (true))
	      down: `((fdmove -c 2 1)
		      (foreground ((mount -o "ro,remount,noexec,nosuid" ,dev /var)))
		      (foreground ((sync)))
		      (foreground ((umount /var)))
		      (true))))))

;; kmsg is a super lightweight syslogd-style service
;; that reads logs from /dev/kmsg and stores them in
;; /var/log/services/kmsg, taking care to compress
;; (and eventually delete) old logs
(define kmsg
  (let ((dir  '/var/log/services/kmsg)
        (opts '(t n10 s1000000 "!zstd -c -"))
        (nfd  3))
    (make-service
     name:   'kmsg
     inputs: (list zstd)
     after:  (list var-mounted-rw)
     spec:   (longrun*
	      notification-fd: nfd
	      run: `((fdmove -c 2 1)
		     (if ((mkdir -p ,dir)))
		     (if ((chown -R catchlog:catchlog ,dir)))
		     (if ((chmod "2700" ,dir)))
		     (pipeline -w ((s6-setuidgid catchlog)
				   (s6-log -d ,nfd -- ,@opts /var/log/services/kmsg)))
		     (redirfd -r 0 /dev/kmsg)
		     (s6-setuidgid catchlog)
		     ;; strip off timestamps and have s6-log prepend tai64n timestamps instead;
		     (sed -e "s/^[0-9].*-;//g"))))))

(define (var-log-services svcs #!key
                          ;; keep at most 10 old logs
                          ;; and rotate at 1MB, using
                          ;; zstd to compress old logs
                          (log-opts '(t n10 s1000000
                                        "!zstd -c -")))
  (let ((logdir     (lambda (svc)
                      (conc
                        "/var/log/services/"
                        (service-name svc))))
        ;; TODO: better detection of logging options,
        ;; compression, etc.
        (log-inputs (if (member "!zstd -c -" log-opts)
                      (list zstd)
                      '())))
    (foldl
      (lambda (lst old)
        ;; only touch longrun services
        ;; that do not already have a producer-for
        (if (or (not (longrun? (service-spec old)))
                (kref* old producer-for:))
          (cons old lst)
          (let ((ospec  (service-spec old))
                (oafter (service-after old))
                (lname  (string->symbol (conc (service-name old) "-log")))
                (dir    (logdir old)))
            (cons
              (make-service
                name:   lname
                inputs: log-inputs
                after:  (list var-mounted-rw)
                spec:   (longrun*
			 consumer-for: (service-name old)
                         dependencies: (conc var-mounted-rw "\n")
                         notification-fd: 3
                         run: `((if ((mkdir -p ,dir)))
                                (if ((chown -R catchlog:catchlog ,dir)))
                                (if ((chmod "2700" ,dir)))
                                (s6-setuidgid catchlog)
                                (exec -c)
                                (s6-log -d 3 ,@log-opts ,dir))))
              (cons
                (update-service
                  old
                  after: (if (memq var-mounted-rw oafter)
			     oafter
			     (cons var-mounted-rw oafter))
                  spec:  (kupdate ospec producer-for: lname))
                lst)))))
      (list kmsg)
      svcs)))

