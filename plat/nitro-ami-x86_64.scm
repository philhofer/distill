(import
 scheme
 (only (chicken module) export)
 (distill base)
 (distill plan)
 (distill execline)
 (distill fs)
 (distill image)
 (distill system)

 (svc getty))

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
   (cdn-artifact "7QIo0Y7kSLUMeyWmL1f-jHmNsG_7iW04nwAhmJ_MTZc=" ".config" #o644)))

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
              (var-mount "/dev/nvme0n1p3")
              ;; ttyS0 is the EC2 console
              (console-root-shell speed: 115200 tty: 'ttyS0))
   packages: (list ami-preboot imgtools)
   mkimage:  (mbr-image "ami-x86_64")))
