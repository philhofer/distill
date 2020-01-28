(define-library (package)
  (import
    (scheme base)
    (scheme write)
    (srfi 26) ;; cut
    (srfi 69) ;; make-hash-table, etc
    (memo)
    (table)
    (eprint)
    (plan))
  (cond-expand
    (chicken (import
               (chicken type)
               typed-records
               (only (chicken base) flatten))))
  (export
    *this-machine*
    build-config
    config->builder
    package->stages
    package?
    make-package
    update-package
    package-label
    package-tools
    package-inputs
    package-build
    package-prebuilt)
  (include "package.scm"))
