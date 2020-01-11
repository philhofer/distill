(define-library (base)
  (import
    (scheme base)
    (srfi 2)
    (srfi 69)
    (memo)
    (plan)
    (execline))
  (cond-expand
    (chicken
      (import
        (only (chicken base) flatten foldl)
        (only (chicken string) conc))
      (import-for-syntax
        (only (chicken io) read-string))))
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
    make
    skalibs
    execline-tools
    gnu-build
    cc-env)
  (include "base.scm"))
