(import
 scheme
 (distill package)
 (distill base))

(define bison
  (cmmi-package
   "bison" "3.7.6"
   "https://ftp.gnu.org/gnu/$name/$name-$version.tar.gz"
   "Raz-R0d5nsU21jrF_WYNcZPzrHrnFRcSxaFxSk_x7kM="
   tools: (list m4 perl)))
