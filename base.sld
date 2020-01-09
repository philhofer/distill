(define-library (base)
  (import
    (scheme base)
    (srfi 69)
    (memo)
    (plan)
    (execline))
  (cond-expand
    (chicken
      (import
        (only (chicken base) flatten foldl)
        (only (chicken string) conc))))
  (export
    pkgs->bootstrap
    libgmp
    libmpfr
    libmpc
    m4
    zlib
    gawk
    bison
    flex
    ncurses
    libisl
    perl
    bzip2
    binutils-for-target
    gnu-build
    cc-env)
  (include "base.scm"))
