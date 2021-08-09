(import
 scheme
 (distill package)
 (distill base))

(define gdbm
  (cmmi-package
   "gdbm" "1.20"
   "https://ftp.gnu.org/gnu/$name/$name-$version.tar.gz"
   "99AsgrSaJ8N-u_4xTD1cYR-5hVPhmKsHoqOuhNLOepo="
   extra-configure: '(--disable-dependency-tracking --enable-fast-install)))
