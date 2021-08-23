(import
 scheme
 (distill plan))

(define ca-certs
  (remote-file
   ;; FIXME: this (obviously) is not stable
   "https://curl.se/ca/cacert.pem"
   "jI5SrvQbQmS5G03pblZzIlI-bgYG2Fq0oDG8Sdji5ec="
   "/etc/ssl/cert.pem" #o644))
