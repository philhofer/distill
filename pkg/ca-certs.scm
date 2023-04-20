(import
 scheme
 (distill plan))

(define ca-certs
  (remote-file
   "https://curl.se/ca/cacert-2023-01-10.pem"
   "6CeyuIIVcSYkNSfxM8lGZcqY_qnzvyhsZtwUlAK4wq8="
   "/etc/ssl/cert.pem" #o644))
