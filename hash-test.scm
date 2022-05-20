(import
 scheme
 (distill hash)
 (distill archive)
 (chicken base)
 (chicken port)
 (chicken file)
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
      slow)
     (test string=? str (with-output-to-string
                          (lambda ()
                            (test string=? (hash-of str)
                                  (copy-port+hash
                                   (open-input-string str)
                                   (current-output-port)))))))
   strs))

(test-same-file-hashes "hash.scm")

(let ((tmpfile "pkgs.tar.zst"))
  (delete-file* tmpfile)
  (let ((outhash (dir->tar.zst "pkg" tmpfile)))
    (test string=? outhash (hash-file tmpfile))
    (delete-file tmpfile)
    (display "archive test OK.\n")))

(let ((tmpfile "fork.pkgs.tar.zst"))
  (delete-file* tmpfile)
  (let ((outhash (fork+dir->tar.zst "pkg" tmpfile)))
    (test string=? outhash (hash-file tmpfile))
    (delete-file tmpfile)
    (display "archive fork test OK.\n")))

(display "hash test OK.\n")
