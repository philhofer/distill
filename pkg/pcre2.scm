(import
 scheme
 (distill package)
 (distill base)
 (pkg libedit))

(define pcre2
  (cmmi-package
   "pcre2" "10.39"
   "https://github.com/PhilipHazel/$name/releases/download/$name-$version/$name-$version.tar.gz"
   "hNbA-qLBCvua54FBvqWv28a6qSnGyp7udmdXkFrBGKg="
   libs: (list libedit zlib libbz2)
   extra-configure: '(--disable-dependency-tracking
                      --enable-pcre2-16
                      --enable-pcre2-32
                      --enable-pcre2grep-libz
                      --enable-pcre2grep-libbz2
                      --with-match-limit-depth=8192)))
