(import
 scheme
 (distill plan))

(define ca-certs
  (remote-file
   ;; FIXME: this (obviously) is not stable
   "https://curl.se/ca/cacert.pem"
   "pGkyd5ZaxZ9Rbealbho2afhls4hd1_iENP598_7CExk="
   "/etc/ssl/cert.pem" #o644))
