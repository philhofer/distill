(import
 scheme
 (distill plan))

(define ca-certs
  (remote-file
   "https://curl.se/ca/cacert-2022-04-26.pem"
   "NB5IiqcOSglBeFfDd1xW95TkNFMPH_q1f_zAZlSDJ7M="
   "/etc/ssl/cert.pem" #o644))
