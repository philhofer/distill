(import
 scheme
 (distill plan))

(define ca-certs
  (remote-file
   ;; FIXME: this (obviously) is not stable
   "https://curl.se/ca/cacert.pem"
   "X-D1GgvJ1OaTrOWYOX4m8smzfqMZQ59ROguxsGnHpgw="
   "/etc/ssl/cert.pem" #o644))
