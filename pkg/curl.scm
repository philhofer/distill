(import
 scheme
 (distill package)
 (distill base))

(define curl
  (cmmi-package
   "curl" "7.85.0"
   "https://curl.haxx.se/download/$name-$version.tar.xz"
   "OrXdEIVKj5S9viFmfnkKBGZZ4P_SGMYsAG2E8Tnhnrs="
   tools: (list perl)
   libs:  (list libressl zlib)
   extra-configure: '(--with-openssl ; works with libressl
                      --with-ca-bundle=/etc/ssl/cert.pem
                      --without-libidn
                      --without-libidn2
                      --without-nghttp2
                      --disable-ldap
                      --without-libssh2
                      --enable-ipv6
                      --enable-unix-sockets)))

(define libcurl (libs curl))
