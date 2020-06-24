(import
  scheme
  (distill package)
  (distill base))

(define bison
  (cmmi-package
   "bison" "3.6.4"
   "https://ftp.gnu.org/gnu/$name/$name-$version.tar.gz"
   "CqWBAC5QpYjWhY0DQg1N1FjfsSWwikuKoS1l_oxPs-o="
   tools: (list m4 perl)))

