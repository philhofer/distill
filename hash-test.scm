(import
  scheme
  (distill hash)
  (chicken base)
  (chicken port)
  (chicken io))

(include "test-helpers.scm")

(define (slow-hash-file f)
  (call-with-input-file f hash-of))

(define (slow-hash-string s)
  (call-with-input-string s hash-of))

(define (slow-hash-string-2 s)
  (with-output-to-hash (lambda () (display s))))

(define (slow-hash-string-3 s)
  (let ((h (new-hasher)))
    (display s (hasher->output-port h))
    (hash-finalize h)))

(define (test-same-file-hashes f)
  (test string=? (slow-hash-file f) (hash-file f)))

(let ((slow (list slow-hash-string slow-hash-string-2 slow-hash-string-3))
      (strs '("" "xyz" "   " "a longer string")))
  (for-each
    (lambda (str)
      (for-each
        (lambda (slow)
          (test string=? (hash-of str) (slow str)))
        slow))
    strs))

(test-same-file-hashes "hash.scm")
(display "hash test OK.\n")
