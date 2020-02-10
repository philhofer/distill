(define-library (distill plan)
  (export
    artifact-dir
    plan-dir
    interned
    interned-symlink
    interned-dir
    local-archive
    remote-archive
    remote-file
    overlay

    build-plan!
    build-graph!
    plan?
    make-plan
    plan-name
    plan-hash
    plan-inputs
    plan-outputs
    load-plan

    short-hash
    artifact?
    artifact-hash
    artifact-format
    artifact-extra

    compute-stages)

  ;; for non-r7rs imports, please try to keep
  ;; the requisite imported functions explicit
  ;; so that it's obvious what needs to be replaced
  ;; for a non-chicken implementation
  (cond-expand
    (chicken
      (import
        matchable
        (chicken type) ;; type hints can be replaced with a no-op
	(only (chicken file) file-exists? directory-exists? move-file copy-file create-directory create-temporary-file create-temporary-directory delete-file* delete-file delete-directory rename-file)
	(only (chicken file posix) file-permissions set-file-permissions! create-symbolic-link file-size)
        (only (chicken base) flatten void)
	(only (chicken io) read-string write-string)
        (only (chicken process-context) current-directory)
	(only (chicken process) process-run process-wait)
	(only (chicken sort) sort)
	(typed-records))))

  (import
    scheme
    (scheme base)
    (scheme read)
    (scheme write)
    (srfi 2) ;; and-let*
    (only (srfi 12) condition? abort)
    (only (srfi 13) string-prefix? string-suffix? string< substring/shared string-any)
    (srfi 26) ;; cut, cute
    (srfi 69) ;; hash tables
    (distill memo)
    (distill nproc)
    (distill filepath)
    (distill sequence)
    (distill eprint)
    (distill coroutine)
    (distill hash))
  (include "plan.scm"))
