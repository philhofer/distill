(import
 scheme
 (distill base))

(define linux-virt-x86_64
  (linux/config-static
   "virt-x86_64"
   (cdn-artifact "FTMQoxE4ClKOWLDdcDVzWt8UuizXfMmR4duX8Z-5qlY=" ".config" #o644)))
