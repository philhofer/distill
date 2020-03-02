(define-library (distill base)
  (import
    (scheme base)
    (scheme write)
    (scheme file)
    (srfi 2)
    (srfi 69)
    (distill table)
    (distill eprint)
    (distill filepath)
    (distill kvector)
    (distill memo)
    (distill plan)
    (distill package)
    (distill execline))
  (cond-expand
    (chicken
      (import
        (only (chicken syntax) er-macro-transformer)
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
    native-toolchain-for
    native-toolchain
    native-binutils
    native-gcc
    busybox-core
    busybox/config
    execline-tools
    gcc-for-target
    cc-for-target
    libssp-nonshared)
  (include "base.scm"))
