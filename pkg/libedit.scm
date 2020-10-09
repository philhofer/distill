(import
 scheme
 (distill package)
 (distill base)
 (pkg ncurses))

(define libedit
  (cmmi-package
   "libedit" "20191231-3.1"
   "https://www.thrysoee.dk/editline/$name-$version.tar.gz"
   "x0NyCF6katZwtz3Wpc8fVKjK8nawKLWVgsdZ2eTgFsU="
   libs: (list ncurses)))
