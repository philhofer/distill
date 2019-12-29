(define-library (hash)
  (export
    hash-u8vector
    hash-bytevector
    hash-string
    hash-file)
  (import
    (scheme base)
    (srfi 4)
    (chicken base)
    (chicken blob)
    (chicken foreign)
    (chicken type))
  (include "hash.scm"))
