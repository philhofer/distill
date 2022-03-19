(import
 scheme
 (distill package)
 (distill base))

(define gdbm
  (cmmi-package
   "gdbm" "1.23"
   "https://ftp.gnu.org/gnu/$name/$name-$version.tar.gz"
   "IUGYNnqYTVx-eGo6H6RdSulY1iPKwLjZOB5w1ABvG_s="
   extra-configure: '(--disable-dependency-tracking --enable-fast-install)))
