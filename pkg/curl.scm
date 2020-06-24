(import
  scheme
  (distill package)
  (distill base))

(define curl
  (cmmi-package
   "curl" "7.71.0"
   "https://curl.haxx.se/download/$name-$version.tar.xz"
   "IEzcN01pG47UmNoDbt_D-VZk3z2-aQUzzInvQ6ny6ow="
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
