(define trace-log (make-parameter #f))

(define (trace . body)
  (when (trace-log)
    (apply info body)))

(define (fatal . args)
  (apply info args)
  (exit 1))

(define (info head . rest)
  (let ((stderr (current-error-port)))
    (let loop ((head head)
               (rest rest))
      (display head stderr)
      (if (null? rest)
        (newline stderr)
          (begin
            (display " " stderr)
            (loop (car rest) (cdr rest)))))))
