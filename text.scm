(define (for-each/intersperse proc lst item)
  (let loop ((lst lst))
    (and (pair? lst)
         (let ((head (car lst))
               (rest (cdr lst)))
           (proc head)
           (unless (null? rest) (proc item))
           (loop rest)))))

;; join-with produces a string
;; by concatenating each element
;; of 'lst' interspersed with 'sep'
(define (join-with sep lst)
  (with-output-to-string
    (lambda ()
      (for-each/intersperse
       display
       lst sep))))

;; tabular displays each element of (map op lst)
;; using wsep as the field separator and lsep
;; as the row separator
(define (tabular op wsep lsep lst)
  (with-output-to-string
    (lambda ()
      (for-each
       (lambda (rw)
         (let ((row (op rw)))
           (if (list? row)
               (for-each/intersperse
                display
                row wsep)
               (display row)))
         (display lsep))
       lst))))

;; (lines lst) produces a string with each
;; element of 'lst' on its own line, with
;; sub-lists interspersed with spaces
(define (lines lst)
  (tabular identity " " "\n" lst))

;; map-lines is equivalent to (lines (map proc lst))
(define (map-lines proc lst)
  (tabular proc " " "\n" lst))
