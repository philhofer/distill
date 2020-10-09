(import
 scheme
 (distill package)
 (distill base))

(define gdbm
  (cmmi-package
   "gdbm" "1.18.1"
   "https://ftp.gnu.org/gnu/$name/$name-$version.tar.gz"
   "7ZcU9Pwu1_5MUbjqtWKn2bMgkx05UMTmzv9LqCjJtmg="
   extra-configure: '(--disable-dependency-tracking --enable-fast-install)))

