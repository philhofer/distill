(import
  scheme
  (distill base)
  (distill package))

(define libsodium
  (cmmi-package
   "libsodium" "1.0.18"
   "https://github.com/jedisct1/libsodium/releases/download/$version-RELEASE/$name-$version.tar.gz"
   "TFTXlQdQAuzi6xUcp4-F7hy0wLzDpioPoC1OJQJjwW4="))
