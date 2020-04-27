
;; it's easiest just to embed the whole blake2
;; implementation into one of the generated C sources...
(foreign-declare "#include \"blake2b-ref.c\"")
(foreign-declare "#include \"simple-hash.h\"")

(: new-hasher (-> u8vector))
(define (new-hasher)
  (let* ((sz    (foreign-value "sizeof(blake2b_state)" unsigned-int))
         (blob  (make-u8vector sz))
         (init! (foreign-lambda* int ((u8vector mem))
                  "C_return(blake2b_init((blake2b_state *)mem, 32));")))
    (if (fx= (init! blob) 0)
      blob
      (error "unexpected error in blake2b_init()"))))

;; hasher->output-port takes a hashing object
;; and produces an output port that calls
;; hash-write! when data is sent to the port
(: hasher->output-port (u8vector -> output-port))
(define (hasher->output-port h)
  (make-output-port
   (lambda (str)
     (hash-write! h str)
      (string-length str))
   (lambda () #t)))

;; with-output-to-hash calls (thunk) with
;; current-output-port set to a hasher;
;; the returned value is the hash of the
;; accumulated output
(: with-output-to-hash ((-> *) -> string))
(define (with-output-to-hash thunk)
  (let ((h (new-hasher)))
    (parameterize ((current-output-port (hasher->output-port h)))
      (thunk))
    (hash-finalize h)))

(define (check-state h)
  (or (fx= (u8vector-length h) (foreign-value "sizeof(blake2b_state)" unsigned-int))
      (error "bad hash state object" h)))

(: hash-write! (u8vector (or string u8vector input-port) -> *))
(define (hash-write! h obj)
  (check-state h)
  (let ((cwrite (foreign-lambda* int ((u8vector self) (scheme-pointer mem) (size_t len))
                  "C_return(blake2b_update((blake2b_state *)self, mem, len));")))
    (cond
      ((string? obj)
       (or (fx= (cwrite h obj (string-length obj)) 0)
           (error "error in blake2b_update()")))
      ((u8vector? obj)
       (or (fx= (cwrite h (u8vector->blob/shared obj) (u8vector-length obj)) 0)
           (error "error in blake2b_update()")))
      ((input-port? obj)
       (let* ((mem (make-string 1024))
              (rd! (lambda ()
                     (read-string! #f mem obj 0))))
         (let loop ((n (rd!)))
           (or (eof-object? n)
               (fx= n 0)
               (begin
                 (or (cwrite h mem n)
                     (error "error in blake2b_update()"))
                 (loop (rd!)))))))
      (else
        (error "hash-write! can't hash object" obj)))))

(: hash-finalize (u8vector -> string))
(define (hash-finalize h)
  (check-state h)
  (let* ((outstr (make-string 44))
         (final! (foreign-lambda* int ((u8vector self) (scheme-pointer dst)) #<<EOF
unsigned char buf[32];
int err = blake2b_final((blake2b_state *)self, buf, 32);
assert(err == 0); hash_to_base64(dst, buf); C_return(0);
EOF
)))
    (final! h outstr)
    outstr))

(: hash-of (* #!rest * -> string))
(define (hash-of x . rest)
  (let ((h (new-hasher)))
    (hash-write! h x)
    (for-each
      (cut hash-write! h <>)
      rest)
    (hash-finalize h)))

;; hash-file returns the hash of a file
;; (if the file exists), or #f if the file
;; doesn't exist, or it throws an error if
;; an I/O error is encountered
(: hash-file (string -> (or string false)))
(define (hash-file fp)
  (let* ((out (make-string 44))
         (res ((foreign-lambda int "fast_hash_file" scheme-pointer nonnull-c-string)
               out fp)))
    (cond
      ((= res 0) out)
      ((= res 2) #f)
      (else
        (call-with-input-file fp hash-of)))))
