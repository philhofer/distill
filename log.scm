(define trace-log (make-parameter #f))

(define-syntax trace
  (syntax-rules ()
    ((_ args* ...)
     (when (trace-log)
       (info args* ...)))))

(define-syntax fatal
  (syntax-rules ()
    ((_ args* ...)
     (begin
       (info args* ...)
       (exit 1)))))

(define (info . args)
  (fmt (current-error-port) (cat (fmt-join dsp args " ") fl)))

