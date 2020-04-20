(import
  scheme
  (distill package)
  (distill base)
  (pkg libedit))

(define pcre2
  (cmmi-package
   "pcre2" "10.34"
   "https://ftp.pcre.org/pub/pcre/$name-$version.tar.gz"
   "dUEXxoMgBjr2H6OHV8jRk-dWOqW-XqOnNeFlYZ3mp3A="
   libs: (list libedit zlib bzip2)
   extra-configure: '(--disable-dependency-tracking
		      --enable-pcre2-16
		      --enable-pcre2-32
		      --enable-pcre2grep-libz
		      --enable-pcre2grep-libbz2
		      --with-match-limit-depth=8192)))
