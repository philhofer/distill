(define-library (distill buildenv)
  (export
    gnu-build
    ska-build

    *this-machine*
    triple
    sysroot
    build-triple
    config-prepend
    configure-args
    cc-env
    cc-env/build
    cc-env/kbuild
    make-env
    makeflags
    pair->string=
    export*
    +cross
    script-apply-patches
    patch*
    strip-binaries-script)
  (import
    scheme
    (scheme base)
    (distill plan)
    (distill memo)
    (distill filepath)
    (distill package)
    (distill execline))
  (cond-expand
    (chicken (import
               (only (chicken string) conc)
               (chicken type))))
  (include "buildenv.scm"))
