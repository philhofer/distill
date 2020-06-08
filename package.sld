(define-library (distill package)
  (import
    scheme
    (srfi 6)  ;; string ports
    (srfi 12)
    (srfi 26) ;; cut
    (srfi 39) ;; parameters
    (srfi 69) ;; make-hash-table, etc
    (distill memo)
    (distill eprint)
    (distill contract)
    (distill coroutine)
    (distill kvector)
    (distill filepath)
    (distill execline)
    (distill plan))
  (cond-expand
    (chicken (import
               (chicken type)
               (chicken keyword)
               (chicken module)
	       (only (chicken process-context)
		     executable-pathname)
               (only (chicken syntax) define-for-syntax)
               (only (chicken string) conc string-translate* string-split)
	       (only (chicken file) file-exists?)
               (only (chicken port)
                     with-output-to-string
                     call-with-output-string)
	       (only (chicken condition) print-error-message)
               (only (chicken base) include error flatten when unless o disjoin foldl))))
  (cond-expand
    (chicken (import-for-syntax
               (chicken keyword))))
  (export
    *this-machine*
    bind
    patchfiles*
    $strip-cmd
    config->builder
    make-config
    config?

    expander
    configure

    expand-package
    url-translate
    cc-package
    cmmi-package
    libs
    binaries
    subpackage

    $arch
    $sysroot
    $triple
    $build
    $native?
    $leaf
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
    $build-triple
    $build-toolchain
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
    $CXXFLAGS
    $LDFLAGS
    $ARFLAGS
    $RANLIB
    $READELF
    cc-env/build
    $cc-env/for-build
    $cc-env/for-kbuild
    spaced
    k=v*
    exports->script)
  (include "package.scm"))
