(module matchable
  (match
   match-lambda
   match-lambda*
   match-let
   match-let*
   match-letrec)
  (import scheme (chicken base) (chicken memory representation))

  ;; CHICKEN-specific glue

  ;; slot-ref type obj n
  ;; Returns the 'n'th field of the record 'obj'.
  ;; 'n' might be a quoted symbol indicating a field name, we have to reject it
  ;; since CHICKEN doesn't carry any information about the field names.
  (define-syntax slot-ref
    (syntax-rules ()
      ((_ type obj n) (if (fixnum? n)
                          (record-instance-slot obj n)
                          (error "Accessing fields by name is not supported")))))

  ;; slot-set! type obj n val
  ;; Sets the value of the 'n'th field of the record 'obj' to 'val'.
  (define-syntax slot-set!
    (syntax-rules ()
      ((_ type obj n val) (record-instance-slot-set! obj n val))))

  ;; is-a? obj type
  ;; Returns #t if 'obj' is a record with name 'type', #f otherwise.
  (define-syntax is-a?
    (syntax-rules ()
      ((_ obj type) (record-instance? obj type))))

  (include-relative "match.scm")
)
