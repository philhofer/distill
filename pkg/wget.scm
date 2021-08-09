(import
 scheme
 (distill package)
 (distill base))

(define wget
  (cmmi-package
   "wget" "1.21.1"
   "https://ftp.gnu.org/gnu/$name/$name-$version.tar.gz"
   "h8qOV881NJqRVDFE5jJtIMkp2sPQjt06oJv-tK0Dy1I="
   tools: (list perl)
   libs:  (list libressl)
   env:   '((OPENSSL_LIBS . "-lssl -lcrypto")
            (OPENSSL_CFLAGS . -lssl))
   extra-configure: '(--disable-dependency-tracking
                      --with-ssl=openssl
                      --with-openssl
                      --without-libidn)))

