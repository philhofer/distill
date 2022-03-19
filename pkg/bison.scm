(import
 scheme
 (distill package)
 (distill base))

(define bison
  (cmmi-package
   "bison" "3.8.2"
   "https://ftp.gnu.org/gnu/$name/$name-$version.tar.gz"
   "LeajJ9QabH7B1yZS0Ahmwx1bVPCFc9jKM4OC5zoJloU="
   tools: (list m4 perl)))
