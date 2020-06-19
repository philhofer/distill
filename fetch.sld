(define-library (distill fetch)
  (export
    fetch
    fetch+hash)
  (import
    scheme
    (distill eprint)
    (distill filepath)
    (distill hash)
    (only (distill coroutine) process-wait/yield))
  (cond-expand
    (chicken (import
	       (chicken type)
	       (only (chicken file) file-exists?)
	       (only (chicken process) process-run)
	       (only (chicken process-context) get-environment-variable)
	       (only (chicken string) string-split)
	       (only (chicken base) include receive delay-force))))
  (include "fetch.scm"))
