(import
  scheme
  (distill plan)
  (distill eprint)
  (distill filepath)
  (srfi 69)
  (chicken file))

;; this script
;;  1) removes plans that never built successfully, and
;;  2) removes artifacts that aren't referenced by any plan

(define (note-inputs f ht)
  (let ((lst (with-input-from-file f read)))
    (or (eof-object? lst)
        (for-each
          (lambda (vec)
            (hash-table-set! ht (vector-ref vec 2) #t))
          lst))))

(define (note-outputs f ht)
  (let ((vec (with-input-from-file f read)))
    (or (eof-object? vec)
        (hash-table-set!
          ht
          (vector-ref vec 1)
          #t))))

;; walk every known plan and mark its inputs and outputs as live;
;; if the plan never produced outputs, it is dead (and can be pruned)
(define live-artifacts
  (find-files
    (plan-dir)
    limit:  0
    seed:   (make-hash-table test: string=? hash: string-hash)
    action: (lambda (dir ht)
              (let ((ofile (filepath-join dir "outputs.scm")))
                (if (file-exists? ofile)
                  (begin
                    (note-outputs ofile ht)
                    (note-inputs (filepath-join dir "inputs.scm") ht))
                  (begin
                    (info "removing dead plan" dir)
                    (delete-directory dir #t))))
              ht)))

;; walk every artifact and produce a list of those
;; not referenced by any plan
(define dead-artifacts
  (find-files
    (artifact-dir)
    limit: 0
    test:  (lambda (f)
             (not (hash-table-ref/default live-artifacts (basename f) #f)))))

(for-each
  (lambda (art)
    (info "deleting" art)
    (delete-file art))
  dead-artifacts)
