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
    artifact-kind
    artifact-path
    artifact-hash
    artifact-format
    artifact-extra)

  ;; for non-r7rs imports, please try to keep
  ;; the requisite imported functions explicit
  ;; so that it's obvious what needs to be replaced
  ;; for a non-chicken implementation
  (cond-expand
    (chicken
      (import
        matchable
        (chicken fixnum)
        (chicken type) ;; type hints can be replaced with a no-op
        (chicken foreign)
	(only (chicken file) file-exists? directory-exists? create-directory create-temporary-file create-temporary-directory delete-file* delete-file delete-directory rename-file)
	(only (chicken file posix) file-permissions set-file-permissions! create-symbolic-link file-size)
        (only (chicken base) include error unless when flatten void current-error-port exit)
	(only (chicken io) read-string write-string)
        (only (chicken process-context) current-directory)
        (only (chicken condition) print-error-message)
	(only (chicken process) process-run)
	(only (chicken sort) sort))))

  (import
    scheme
    (srfi 2)  ;; and-let*
    (srfi 6)  ;; string ports
    (srfi 11) ;; let-values
    (srfi 12)
    (only (srfi 13) string-prefix? string-suffix? string< substring/shared string-any)
    (srfi 26) ;; cut, cute
    (srfi 39) ;; parameters
    (srfi 69) ;; hash tables
    (distill memo)
    (distill nproc)
    (distill filepath)
    (distill sequence)
    (distill eprint)
    (distill coroutine)
    (distill contract)
    (distill kvector)
    (distill hash))
  (include "plan.scm"))
