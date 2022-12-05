(import
 scheme
 (distill base)
 (distill image)
 (distill system))

(define container-rootfs
  (make-platform
   config:   (default-config 'x86_64) ;; FIXME: multi-arch
   kernel:   #f
   cmdline:  '()
   packages: '()
   mkimage:  (container-rootfs-image "container-x86_64")))
