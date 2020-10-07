(import
  scheme
  (distill package)
  (distill base))

(define bison
  (cmmi-package
   "bison" "3.7.2"
   "https://ftp.gnu.org/gnu/$name/$name-$version.tar.gz"
   "Dki4-d9OhRdte4oRORDCoXvYOoaQoUOhVQbTsPZOAkc="
   tools: (list m4 perl)))

