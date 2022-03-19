(import
 scheme
 (distill package)
 (distill base))

(define curl
  (cmmi-package
   "curl" "7.82.0"
   "https://curl.haxx.se/download/$name-$version.tar.xz"
   "pjt2yQG2ZCGpWr9wTt9OqFrEWwHcDq-s4ef-8zfthdc="
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
