(import
 scheme
 (distill package)
 (distill base))

(define curl
  (cmmi-package
   "curl" "7.72.0"
   "https://curl.haxx.se/download/$name-$version.tar.xz"
   "lD1m909FQHLUJ676BCg9c8mpX2d2zGy39e-UoNFJ-2I="
   tools: (list perl)
   libs:  (list libressl zlib)
   extra-configure: '(--with-ca-bundle=/etc/ssl/cert.pem
                      --without-libidn
                      --without-libidn2
                      --without-nghttp2
                      --disable-ldap
                      --without-libssh2
                      --enable-ipv6
                      --enable-unix-sockets)))

(define libcurl (libs curl))
