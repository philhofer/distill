(import
  scheme
  (only (chicken file) find-files)
  (distill eprint)
  (distill filepath)
  (distill plan)
  (distill hash))

(define (bad-files dir)
  (find-files
    dir
    action: (lambda (f lst)
              (let ((base (basename f))
                    (h    (hash-file f)))
                (or (equal? base h)
                    (info "file" f "hash unexpected hash" h))
                lst))))

(bad-files (artifact-dir))
