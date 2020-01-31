(define-library (distill package)
  (import
    (scheme base)
    (scheme write)
    (srfi 26) ;; cut
    (srfi 69) ;; make-hash-table, etc
    (distill memo)
    (distill table)
    (distill eprint)
    (distill execline)
    (distill plan))
  (cond-expand
    (chicken (import
               (chicken type)
               typed-records
               (only (chicken port)
                     with-output-to-string
                     call-with-output-string)
               (only (chicken base) flatten))))
  (export
    *this-machine*
    build-config
    default-config
    config->builder
    package->stages
    package?
    make-package
    update-package
    package-label
    package-tools
    package-inputs
    package-build
    package-prebuilt
    make-recipe
    update-recipe
    recipe?
    recipe-env
    recipe-script)
  (include "package.scm"))
