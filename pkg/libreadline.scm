(import
  scheme
  (distill package)
  (distill base)
  (pkg ncurses))

(define libreadline
  (cmmi-package
   "readline" "8.0"
   "https://ftp.gnu.org/gnu/$name/$name-$version.tar.gz"
   "uIhQtdWX5EA75LisbuLAQyi1mcLYNydQCBEhEH3YcO4="
   libs: (list ncurses)))

