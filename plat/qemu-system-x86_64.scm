(import
  scheme
  (distill base)
  (distill plan)
  (distill execline)
  (distill fs)
  (distill image)
  (distill system)
  (pkg linux-virt-x86_64))

;; qemu-preboot formats the tail end of /dev/vda
;; as the /var partition and then reboots so that
;; the kernel can pick up the new partition
(define qemu-preboot
  (interned
   "/sbin/preboot" #o700
   (lambda ()
     (write-exexpr
      '((if ((test -b /dev/vda2))) ; sanity
	(if -t -n ((test -b /dev/vda3)))
	(foreground ((echo "re-partitioning /dev/vda...")))
	(if ((dosextend -n3 -k /dev/vda)))
	(test -b /dev/vda3))))))

;; qemu-system-x86_64 is a platform for KVM-accelerated
;; linux guests; the presumption here is that we're booting
;; using SeaBIOS and that the root disk is provided via the
;; virtio interface
;;
;; NOTE: the presumption re. the bootable image produced
;; is that it is truncate(1)'d to the desired size before booting
(define qemu-system-x86_64
  (make-platform
   config:   (default-config 'x86_64)
   kernel:   linux-virt-x86_64
   cmdline:  '("root=/dev/vda2" "rootfstype=squashfs" "console=ttyS0")
   services: (list (var-mount "/dev/vda3"))
   packages: (list qemu-preboot imgtools)
   mkimage:  (mbr-image "qemu-x86_64")))
