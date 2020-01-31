(import
  (scheme base)
  (distill hash)
  (chicken base)
  (chicken io))

(include "test-helpers.scm")

(define (slow-hash-file f)
  (call-with-input-file f (lambda (prt) (hash-string (read-string #f prt)))))

(define (test-same-file-hashes f)
  (test string=? (slow-hash-file f) (hash-file f)))

(test-same-file-hashes "hash.scm")
