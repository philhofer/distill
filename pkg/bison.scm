(import
  scheme
  (distill package)
  (distill base))

(define bison
  (cmmi-package
   "bison" "3.5.2"
   "https://ftp.gnu.org/gnu/$name/$name-$version.tar.gz"
   "RzNc3Yv-A6hUD9trf_dAdZRKF_Lhvwmpw6r_UAtVyY0="
   tools: (list m4 perl)))

