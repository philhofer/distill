(import
 scheme
 (only (chicken port) with-output-to-string)
 (only (chicken module) export)
 (distill base)
 (distill plan)
 (distill execline)
 (distill fs)
 (distill image)
 (distill service)
 (distill execline)
 (distill system)

 (pkg ip-wait)
 (pkg wget)
 (svc getty)
 (svc acpid))

;; Amazon EC2 Nitro-based platform
;;
;; Images that use the nitro-ami-* platform
;; can be imported as EBS snapshots and then
;; converted to AMIs. The disk partitions will
;; automatically be updated on the first boot
;; to reflect the size of the volume.

;; the disk layout is
;;   partition 1: kernel
;;   partition 2: root
;;   partition 3 (doesn't exist on first boot): /var
(define rootdev "/dev/nvme0n1")

(define (rootpart n)
  (string-append rootdev "p" (number->string n)))

;; ami-preboot formats the root device
;; so that the tail contains the /var mount
(define ami-preboot
  (interned
   "/sbin/preboot" #o700
   (lambda ()
     (write-exexpr
      `(if (test -b ,(rootpart 2)) ; sanity
           if -t -n (test -b ,(rootpart 3))
           foreground (echo "re-partitioning root device")
           if (dosextend -n3 -k ,rootdev)
           test -b ,(rootpart 3))))))

;; default kernel image; can be overridden
;; either by manipulating the nitro-ami-* platform
;; object or with a (set! linux-ami-x86_64 ...)
(export linux-ami-x86_64)
(define linux-ami-x86_64
  (linux/config-static
   "ami-x86_64"
   (cdn-artifact "DqJkb_vWTMFFmqHAi72-TozfhoeU1Rmptca9c8pCvjM=" ".config" #o644)))

;; write a script called ec2-meta
;; which is invoked like
;;   ec2-meta public-keys/0/openssh-key
;; in order to deduplicate code that needs to
;; access ec2 metadata
(define bin/ec2-meta
  (interned "/bin/ec2-meta" #o755
            (with-output-to-string
              (lambda ()
                (write-exexpr
                 `(foreground
                   ;; block until the default route is available
                   (redirfd -w 1 /dev/null ip-wait route "^default")
                   backtick -E token
                   (wget -q --method=PUT -O "-"
                         "--header=X-aws-ec2-metadata-token-ttl-seconds: 21600"
                         http://169.254.169.254/latest/api/token)
                   wget -q -O "-" "--header=X-aws-ec2-metadata-token: ${token}"
                   "http://169.254.169.254/latest/meta-data/${1}")
                 shebang: "#!/bin/execlineb -s1")))))

;; ec2-ssh-keys extracts ssh keys from
;; the ec2 metadata service and sticks them
;; into /run/ec2-authorized-keys, which is what
;; the /root/.ssh/authorized_keys will point
;; to (as a symlink)
;;
;; BUGS: only fetches key 0
(define ec2-ssh-keys
  (make-service
   name: 'ec2-ssh-keys
   inputs: (list
            wget
            ip-wait
            bin/ec2-meta
            (interned-symlink "/root/.ssh/authorized_keys" "/run/ec2-authorized-keys"))
   spec:   (oneshot*
            up: `(/bin/umask
                  "077"
                  redirfd -w 1 /run/ec2-authorized-keys
                  s6-setuidgid nobody
                  ec2-meta public-keys/0/openssh-key))))

;; ec2-hostname sets the hostname based
;; on the metadata service "local-hostname" variable
(define ec2-hostname
  (make-service
   name:   'ec2-hostname
   inputs: (list wget ip-wait bin/ec2-meta)
   spec:   (oneshot*
            up: `(backtick
                  -E hostname
                  (s6-setuidgid nobody ec2-meta local-hostname)
                  hostname "$hostname"))))

;; nitro-ami-x86_64 is a basic AMI platform
;; for amazon EC2 images that will run
;; on any Nitro-backed x86_64 instance type
(define nitro-ami-x86_64
  (make-platform
   config:   (default-config 'x86_64)
   kernel:   linux-ami-x86_64
   cmdline:  `(,(string-append "root=" (rootpart 2))
               "rootfstype=squashfs" "console=ttyS0")
   services: (list
              ec2-hostname
              ec2-ssh-keys
              ;; EC2 termination begins with
              ;; an ACPI poweroff event
              acpid
              (var-mount "/dev/nvme0n1p3")
              ;; ttyS0 is the EC2 console
              (console-root-shell speed: 115200 tty: 'ttyS0))
   packages: (list ami-preboot imgtools)
   mkimage:  (mbr-image "ami-x86_64")))
