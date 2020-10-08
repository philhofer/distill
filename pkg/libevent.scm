(import
 scheme
 (distill package)
 (distill base))

(define libevent
  (cmmi-package
   "libevent" "2.1.12-stable"
   "https://github.com/libevent/$name/releases/download/release-$version/$name-$version.tar.gz"
   "ioPtdMKtDH3-2J6yutZwlViKenLek62m02ubrKilqGc="
   libs: (list libressl)))
