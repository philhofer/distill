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
    sub-archive
    overlay

    make-input
    input-link
    input-basedir
    input-wrap
    input?

    fetch-graph!
    build-plan!
    build-graph!
    plan?
    make-plan
    plan-name
    plan-hash
    plan-inputs
    plan-outputs
    plan-null-build?
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
        (only (chicken string) conc)
	(only (chicken file) file-exists? directory-exists? create-directory create-temporary-file create-temporary-directory delete-file* delete-file delete-directory rename-file)
	(chicken file posix)
        (only (chicken base) include error unless when flatten void current-error-port exit identity disjoin)
	(only (chicken io) read-string write-string read-line)
        (only (chicken port) make-broadcast-port)
        (only (chicken process-context) current-directory)
        (only (chicken condition) print-error-message)
	(only (chicken time) current-milliseconds)
	(chicken process)
	(only (chicken sort) sort))))

  (import
    scheme
    (srfi 2)  ;; and-let*
    (srfi 6)  ;; string ports
    (srfi 11) ;; let-values
    (srfi 12)
    (srfi 26) ;; cut, cute
    (srfi 39) ;; parameters
    (srfi 69) ;; hash tables
    (distill memo)
    (distill nproc)
    (distill filepath)
    (distill eprint)
    (distill coroutine)
    (distill contract)
    (distill kvector)
    (distill tai64)
    (distill hash))
  (include "plan.scm"))
