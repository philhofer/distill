(define-library (distill fetch)
  (export
    ;; user-fetch-hook is a parameter
    ;; that can be set in order to have
    ;; plan resolution fetch objects
    ;; from a custom source as they are demanded.
    ;; user-fetch-hook should be a procedure of
    ;; one argument, which will be the hash of
    ;; the requested artifact; the return value
    ;; of the procedure can be a string, in which
    ;; case it will be interpreted as the URL of
    ;; the object, or another procedure, in which
    ;; case that procedure will be called with the
    ;; destination filepath for the artifact
    user-fetch-hook
    ;; symlink-from-directory can be supplied as
    ;; (user-fetch-hook (symlink-from-directory "dir"))
    symlink-from-directory
    fetch
    fetch-artifact)
  (import
    scheme
    (srfi 12) ;; current-exception-handler
    (srfi 39) ;; make-parameter
    (srfi 69)
    (distill eprint)
    (distill filepath)
    (distill hash)
    (distill eprint)
    (only (distill coroutine) process-wait/yield with-locked-key make-keyed-lock))
  (cond-expand
    (chicken
      (import
	    (chicken type)
	    (only (chicken file) file-exists? delete-file delete-file* rename-file)
        (only (chicken file posix) create-symbolic-link)
	    (only (chicken process) process-run)
	    (only (chicken base) include receive delay-force error call/cc when))))
  (include "fetch.scm"))
