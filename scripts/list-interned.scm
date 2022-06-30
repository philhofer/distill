(import
 scheme
 (distill plan)
 (distill filepath)
 (chicken io)
 (chicken file)
 (chicken base)
 (chicken process-context))

(define (walk-for-strings form)
  (cond
   ((pair? form)
    (begin
      (walk-for-strings (car form))
      (walk-for-strings (cdr form))))
   ((vector? form)
    (let loop ((i   0)
               (len (vector-length form)))
      (or (= len i)
          (begin
            (walk-for-strings (vector-ref form i))
            (loop (+ i 1) len)))))
   ((string? form)
    (when (and (= (string-length form) 44)
               (file-exists? (filepath-join (artifact-dir) form)))
      (print form)))
   (else #f)))

(define (walk-file name)
  (with-input-from-file name
    (lambda ()
      (let lp ((form (read)))
        (or (eof-object? form)
            (begin
              (walk-for-strings form)
              (lp (read))))))))

;; these files are known to have interned blobs:
(walk-file "base.scm")
(for-each walk-file (glob "pkg/*.scm"))
(for-each walk-for-strings
          (append
           (include "prebuilt-x86_64.scm")
           (include "prebuilt-aarch64.scm")
           (include "prebuilt-ppc64le.scm")))
