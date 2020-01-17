(define-library (base)
  (import
    (scheme base)
    (scheme write)
    (scheme file)
    (srfi 2)
    (srfi 69)
    (table)
    (log)
    (filepath)
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
    bootstrap-base!
    musl
    libgmp
    libmpfr
    libmpc
    m4
    zlib
    gawk
    byacc
    reflex
    libisl
    bzip2
    make
    skalibs
    binutils-for-target
    gcc-for-target
    busybox-core
    execline-tools
    gnu-build
    gcc-for-target
    cc-env)
  (include "base.scm"))
