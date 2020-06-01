(import
  scheme
  (distill base))

(define linux-virt-x86_64
  (linux/config-static
   "virt-x86_64"
   "patches/linux/config.virt.x86_64"))
