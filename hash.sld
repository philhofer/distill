(define-library (distill hash)
  (export
    hash-u8vector
    hash-bytevector
    hash-string
    hash-file)
  (import
    scheme
    (srfi 4))
  (cond-expand
    (chicken
      (import
        (only (chicken base) include error)
	(only (chicken blob) blob->string string->blob)
	(chicken foreign)
	(chicken type))))
  (include "hash.scm"))
