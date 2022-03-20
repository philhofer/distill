(import
 scheme
 (distill package)
 (distill base)
 (pkg ncurses))

(define libedit
  (cmmi-package
   "libedit" "20210910-3.1"
   "https://www.thrysoee.dk/editline/$name-$version.tar.gz"
   "3PDuSTQnRT_7KFO5DPdicd0dCLOGljk0TbECeoYle04="
   libs: (list ncurses)))
