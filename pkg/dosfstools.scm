(import
  scheme
  (distill base)
  (distill package))

(define dosfstools
  (cmmi-package
   "dosfstools" "4.1"
   "https://github.com/dosfstools/$name/releases/download/v$version/$name-$version.tar.gz"
   "a1HIBRNaQ-39GorXwCXTMLs-rZjAqaQNf0cbD-iqRr4="
   libs: (list linux-headers)
   extra-configure: '(--without-udev --without-iconv)))
