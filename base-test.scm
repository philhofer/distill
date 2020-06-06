(import
  scheme
  (distill plan)
  (distill base)
  (distill package))

(include "test-helpers.scm")

(define bc (force default-build-config))
(define tc (default-config
	     (case *this-machine*
	       ((x86_64) 'aarch64)
	       (else 'x86_64))))
(define exb (expander bc))
(define ext (expander tc))

(let ((mpb (exb musl))
      (mpt (ext musl)))
  (test eq? mpb (exb musl))
  (test eq? #f (plan-null-build? mpb))
  (test eq? mpt (ext musl)))

