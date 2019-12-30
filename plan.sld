(define-library (plan)
  (export
    table
    table->proc
    insert!
    lookup
    make-leaf
    leaf?
    leaf-name
    leaf-src
    leaf-hash
    leaf-format
    update-leaf
    leaf->alist
    alist->leaf
    make-plan
    plan?
    plan-name
    plan-recipe
    plan-inputs
    plan-hash
    plan-outputs
    update-plan
    plan->alist
    alist->plan
    make-interned-file
    interned-file?
    interned-file-abspath
    interned-file-hash
    interned-file-mode
    interned-file-usr
    interned-file-grp
    update-interned-file
    interned-file->alist
    alist->interned-file
    make-recipe
    recipe-env
    recipe-script
    update-recipe
    recipe->alist
    alist->recipe
    make-package
    package?
    package-label
    package-src
    package-tools
    package-inputs
    package-build
    package-overlay
    update-package
    build-package!
    filepath-join
    memoize-eq)
  (import (scheme)
  	  (scheme base)
  	  (scheme read)
          (scheme write)
	  (scheme case-lambda)
          (srfi 2)
          (srfi 9)
	  (srfi 13)
	  (srfi 26)
	  (srfi 28)
	  (srfi 69)
	  (chicken type)
	  (chicken file) ;; TODO: portable!
	  (chicken file posix)
	  (chicken sort)
	  (chicken string)
	  (chicken process)
	  (chicken process-context)
	  (typed-records)
	  (filepath)
	  (log)
	  (execline)
	  (hash))
  (include "plan.scm"))
