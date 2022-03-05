(import
 scheme
 (distill plan))

(define ca-certs
  (remote-file
   ;; FIXME: this (obviously) is not stable
   "https://curl.se/ca/cacert.pem"
   "mS2foynIlJ6B08LZmkOyQYd1NRtQl0w2spRFLuuWsdY="
   "/etc/ssl/cert.pem" #o644))
