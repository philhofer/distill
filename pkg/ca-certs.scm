(import
 scheme
 (distill plan))

(define ca-certs
  (remote-file
   "https://curl.se/ca/cacert-2022-02-01.pem"
   "mS2foynIlJ6B08LZmkOyQYd1NRtQl0w2spRFLuuWsdY="
   "/etc/ssl/cert.pem" #o644))
