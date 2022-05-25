(import
 scheme
 (only (chicken io) write-string)
 (only (chicken file) delete-file)
 (distill fetch)
 (distill hash)
 (distill eprint))

(define my-content "here is a string!\n")
(define content-hash (hash-of my-content))

(define (my-hook hash)
  (if (equal? hash content-hash)
      (lambda (dst)
        (with-output-to-file dst
          (lambda ()
            (write-string my-content))))
      (error "unexpected hash passed to my-hook" hash)))

(let* ((on-fail (lambda ()
                  (error "called on-failure")))
       (src     "https://b2cdn.sunfi.sh/path/does/not/exist"))
  (parameterize ((user-fetch-hook my-hook))
    (fetch-artifact src "." content-hash on-fail)
    (unless (equal? (hash-file content-hash) content-hash)
      (error "unexpected content hash" (hash-file content-hash)))
    (delete-file content-hash)))
