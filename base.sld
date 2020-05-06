(define-library (distill base)
  (import
    scheme
    (srfi 2)
    (srfi 26) ; cut/cute
    (srfi 69)
    (srfi 88)
    (distill eprint)
    (distill filepath)
    (distill kvector)
    (distill text)
    (distill memo)
    (distill plan)
    (distill package)
    (distill execline))
  (cond-expand
    (chicken
      (import
        (only (chicken syntax) er-macro-transformer)
        (only (chicken port) with-output-to-string)
        (only (chicken base) include error flatten foldl unless)
        (only (chicken string) conc))
      (import-for-syntax
        (only (chicken io) read-string))))
  (export
    bootstrap-base!
    ; libc:
    musl
    libssp-nonshared

    ; config:
    default-config
    gcc+musl-static-config

    ; gcc deps:
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

    ; busybox:
    busybox-core
    busybox-full
    busybox/config
    hard

    ; helper package lists:
    binutils-for-triple
    gcc-for-triple
    cc-for-target
    native-toolchain

    ; skaware
    skalibs
    execline-tools
    s6
    s6-rc

    ; execline helpers
    exportall

    ; linux:
    perl
    libelf
    linux-headers
    linux/config-static
    uboot/config

    ; networking:
    libmnl
    libnftnl
    iptables
    iproute2

    ; compression/archive tools
    xz-utils
    lz4
    zstd
    libarchive

    ; ssl
    libressl

    ; filesystem tools
    e2fsprogs
    squashfs-tools
    )
  (include "base.scm"))
