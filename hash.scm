
;; it's easiest just to embed the whole blake2
;; implementation into one of the generated C sources...
(foreign-declare "#include \"blake2b-ref.c\"")
(foreign-declare "#include \"simple-hash.h\"")

(define raw-hash!
  (foreign-lambda void "simple_hash" u8vector u8vector size_t))

(define raw-hash-file!
  (foreign-lambda int "simple_hash_file" u8vector nonnull-c-string))

(: buf->string (u8vector --> string))
(define (buf->string x)
  (blob->string (u8vector->blob/shared x)))

(: string->buf (string --> u8vector))
(define (string->buf x)
  (blob->u8vector/shared (string->blob x)))

(: hash-u8vector (u8vector --> string))
(define (hash-u8vector vec)
  (let ((out (make-u8vector 44)))
    (raw-hash! out vec (u8vector-length vec))
    (buf->string out)))

(define hash-bytevector hash-u8vector)

(: hash-string (string --> string))
(define (hash-string str)
  (hash-u8vector (string->buf str)))

;; hash-file returns the hash of a file
;; (if the file exists), or #f if the file
;; doesn't exist, or it throws an error if
;; an I/O error is encountered
(: hash-file (string -> (or string false)))
(define (hash-file fp)
  (let* ((out (make-u8vector 44))
	 (res (raw-hash-file! out fp)))
    (cond
      ((= res 0) (buf->string out))
      ((= res 2) #f)
      (else      (error "hash-file: unexpected error(errno):" res)))))
