(import
  scheme
  (distill package)
  (distill base)
  (pkg libedit))

(define pcre2
  (cmmi-package
   "pcre2" "10.35"
   "https://ftp.pcre.org/pub/pcre/$name-$version.tar.gz"
   "p1dGJ_gCevrQShXVy97zo2GW_1z4nBQmWTHx7l3goFY="
   libs: (list libedit zlib libbz2)
   extra-configure: '(--disable-dependency-tracking
		      --enable-pcre2-16
		      --enable-pcre2-32
		      --enable-pcre2grep-libz
		      --enable-pcre2grep-libbz2
		      --with-match-limit-depth=8192)))
