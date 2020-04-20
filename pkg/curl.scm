(import
  scheme
  (distill package)
  (distill base))

(define curl
  (cmmi-package
   "curl" "7.69.1"
   "https://curl.haxx.se/download/$name-$version.tar.xz"
   "SE9rx8lnJG7UyJMz8kbJ8affBfFBp4YCMwLwNB_1ErE="
   tools: (list perl)
   libs:  (list libressl zlib)
   extra-configure: '(--without-libidn
		      --without-libidn2
		      --without-nghttp2
		      --disable-ldap
		      --without-libssh2
		      --enable-ipv6
		      --enable-unix-sockets)))
