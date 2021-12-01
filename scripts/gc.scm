(import
  scheme
  (srfi 69)
  (distill filepath)
  (distill plan)
  (chicken file)
  (chicken file posix))

(define (outlink? f)
  (and (symbolic-link? f)
       (let ((base (basename f)))
         (string=? (substring base 0 7) "output:"))))

(define (hash? x)
  (and (string? x)
       (= (string-length x) 44)))

(define live-artifacts
  (let ((art-dir (artifact-dir))
        (pln-dir (plan-dir)))
    (find-files
     pln-dir
     seed: (make-hash-table hash: string-hash test: string=?)
     test: outlink?
     action: (lambda (link ht)
               (let* ((realdir (read-symbolic-link link))
                      (plnhash (basename realdir))
                      (outhash (substring (basename link) (string-length "output:"))))
                 (unless (hash? plnhash)
                   (error "weird hash" plnhash))
                 (unless (hash? outhash)
                   (error "weird artifact hash" outhash))
                 (hash-table-set! ht outhash #t)
                 (hash-table-set! ht plnhash #t)
                 (with-input-from-file
                     (filepath-join art-dir plnhash)
                   (lambda ()
                     (for-each
                      (lambda (in)
                        (unless (hash? (vector-ref in 2))
                          (error "weird input hash" (vector-ref in 2)))
                        (hash-table-set! ht (vector-ref in 2) #t))
                      (read)))))
               ht))))

(find-files
 (artifact-dir)
 test: (lambda (f)
         (not (hash-table-ref/default live-artifacts (basename f) #f)))
 action: (lambda (f lst)
           (print "removing " f)
           (delete-file* f)))
