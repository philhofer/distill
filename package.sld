(define-library (distill package)
  (import
    scheme
    (srfi 6)  ;; string ports
    (srfi 26) ;; cut
    (srfi 39) ;; parameters
    (srfi 69) ;; make-hash-table, etc
    (distill memo)
    (distill eprint)
    (distill contract)
    (distill kvector)
    (distill filepath)
    (distill execline)
    (distill plan))
  (cond-expand
    (chicken (import
               (chicken type)
               (chicken keyword)
               (chicken module)
               (only (chicken syntax) define-for-syntax)
               (only (chicken string) conc string-translate* string-split)
               (only (chicken port)
                     with-output-to-string
                     call-with-output-string)
               (only (chicken base) include error flatten unless o disjoin foldl))))
  (cond-expand
    (chicken (import-for-syntax
               (chicken keyword))))
  (export
    *this-machine*
    patch*
    script-apply-patches
    strip-binaries-script
    $strip-cmd
    build-config
    config->builder
    make-config
    vargs
    csubst
    cmd*

    make-meta-package
    meta-package?
    package?
    make-package
    update-package
    package-label
    package-tools
    package-inputs
    package-build
    package-prebuilt


    url-translate
    cc-package
    cmmi-package

    $arch
    $sysroot
    $triple
    $cc-toolchain
    $native-toolchain
    make-cc-toolchain
    cc-toolchain-env
    make-cc-env
    cc-toolchain-tools
    cc-toolchain-libc
    triple->arch
    triple->sysroot
    $make-overrides
    $cc-env
    build-triple
    $build-triple
    $cross-compile
    $build-CC
    $build-LD
    $build-CFLAGS
    $CC
    $CXX
    $AR
    $LD
    $NM
    $CFLAGS
    $LDFLAGS
    $ARFLAGS
    $RANLIB
    $READELF
    cc-env/build
    cc-env/for-build
    cc-env/for-kbuild
    spaced
    splat
    k=v*
    kvargs
    kvexport)
  (include "package.scm"))
