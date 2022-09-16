(import
 scheme
 (distill plan))

(define ca-certs
  (remote-file
   "https://curl.se/ca/cacert-2022-07-19.pem"
   "d3dZLLRqELZJGQqX299v5FX4RzkO86ft3Xsgo3QcR5Q="
   "/etc/ssl/cert.pem" #o644))
