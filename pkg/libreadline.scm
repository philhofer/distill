(import
 scheme
 (distill package)
 (distill base)
 (pkg ncurses))

(define libreadline
  (cmmi-package
   "readline" "8.1"
   "https://ftp.gnu.org/gnu/$name/$name-$version.tar.gz"
   "Zy87rjG6dQ5aefwkUg70ikMHq0vDrwrDx-uh6lGqIP8="
   libs: (list ncurses)))
