(import
 scheme
 (distill package)
 (distill base))

(define wget
  (cmmi-package
   "wget" "1.20.3"
   "https://ftp.gnu.org/gnu/$name/$name-$version.tar.gz"
   "G3jWdBQ-_4BxoX6NpCl6-mbpV_NH0BEOZZRMLKqlTrs="
   tools: (list perl)
   libs:  (list libressl)
   env:   '((OPENSSL_LIBS . "-lssl -lcrypto")
            (OPENSSL_CFLAGS . -lssl))
   extra-configure: '(--disable-dependency-tracking
                      --with-ssl=openssl
                      --with-openssl
                      --without-libidn)))

