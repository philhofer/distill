(import
 scheme
 (distill package)
 (distill base))

(define libevent
  (cmmi-package
   "libevent" "2.1.12-stable"
   "https://github.com/libevent/$name/releases/download/release-$version/$name-$version.tar.gz"
   "ioPtdMKtDH3-2J6yutZwlViKenLek62m02ubrKilqGc="
   ;; need a compatibility patch for libressl >=3.5:
   ;; https://github.com/openbsd/ports/commit/0366ba542eeb753e9c13ff188609af011bab6b3c
   prepare: '(sed "-i" "43s/$/ \\&\\& LIBRESSL_VERSION_NUMBER < 0x30500000L/" openssl-compat.h)
   libs: (list libressl)))
