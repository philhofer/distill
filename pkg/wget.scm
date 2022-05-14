(import
 scheme
 (distill package)
 (distill base))

(define wget
  (cmmi-package
   "wget" "1.21.3"
   "https://ftp.gnu.org/gnu/$name/$name-$version.tar.gz"
   "ut4GWWhjW8qKEQiJnyhzHa1BcAmalI02hePUL9Wwu9U="
   tools: (list perl)
   libs:  (list libressl)
   env:   '((OPENSSL_LIBS . "-lssl -lcrypto")
            (OPENSSL_CFLAGS . -lssl))
   extra-configure: '(--disable-dependency-tracking
                      --with-ssl=openssl
                      --with-openssl
                      --without-libidn)))

