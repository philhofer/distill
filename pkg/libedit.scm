(import
 scheme
 (distill package)
 (distill base)
 (pkg ncurses))

(define libedit
  (cmmi-package
   "libedit" "20210714-3.1"
   "https://www.thrysoee.dk/editline/$name-$version.tar.gz"
   "71HMmylRbiksKVqK78EDTU2LS6_0zP5QLT_IgbMm8m8="
   libs: (list ncurses)))
