(define-library (plan)
  (export
    artifact-dir
    plan-dir
    make-leaf
    leaf?
    leaf-name
    leaf-src
    leaf-hash
    leaf-format
    update-leaf
    make-interned-file
    interned
    interned-file?
    interned-file-abspath
    interned-file-hash
    interned-file-mode
    interned-file-usr
    interned-file-grp
    update-interned-file
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
    memoize-eq)

  ;; for non-r7rs imports, please try to keep
  ;; the requisite imported functions explicit
  ;; so that it's obvious what needs to be replaced
  ;; for a non-chicken implementation
  (cond-expand
    (chicken
      (import
        (chicken type) ;; type hints can be replaced with a no-op
	(only (chicken file) file-exists? move-file copy-file create-directory find-files create-temporary-directory delete-file* delete-file delete-directory rename-file)
	(only (chicken file posix) file-type file-permissions set-file-permissions! create-symbolic-link read-symbolic-link file-size)
        (only (chicken base) flatten)
	(only (chicken io) read-string write-string)
	(only (chicken process) process-run process-wait)
	(only (chicken sort) sort)
	(typed-records))))

  (import (scheme)
  	  (scheme base)
  	  (scheme read)
          (scheme write)
	  (scheme case-lambda)
	  (srfi 2) ;; and-let*
	  (only (srfi 13) string-prefix? string-suffix? string< substring/shared string-any)
	  (srfi 26) ;; cut, cute
	  (srfi 69) ;; hash tables
	  (table)
	  (filepath)
	  (log)
	  (execline)
	  (hash))
  (include "plan.scm"))
