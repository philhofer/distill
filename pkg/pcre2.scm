(import
 scheme
 (distill package)
 (distill base)
 (pkg libedit))

(define pcre2
  (cmmi-package
   "pcre2" "10.37"
   "https://ftp.pcre.org/pub/pcre/$name-$version.tar.gz"
   "LxMxFsAM7PGZw1RkNNIsJpMCKESjMGcl5mInkzqNR5E="
   libs: (list libedit zlib libbz2)
   extra-configure: '(--disable-dependency-tracking
                      --enable-pcre2-16
                      --enable-pcre2-32
                      --enable-pcre2grep-libz
                      --enable-pcre2grep-libbz2
                      --with-match-limit-depth=8192)))
