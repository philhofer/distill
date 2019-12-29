(import
  (scheme base)
  (hash)
  (chicken base)
  (chicken io))

(define (slow-hash-file f)
  (call-with-input-file f (lambda (prt) (hash-string (read-string #f prt)))))

(define (test-same-file-hashes f)
  (let ((a (slow-hash-file f))
	(b (hash-file f)))
    (or (string=? a b)
	(error "hashes not equal:" a b))))

(test-same-file-hashes "hash.scm")
