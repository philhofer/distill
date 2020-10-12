(foreign-declare "#include \"tai64.inc.h\"")

(: tai64n-now (-> u8vector))
(define (tai64n-now)
  (let ((out      (make-u8vector 12))
        (gettime! (foreign-lambda int "tai64n_gettime" u8vector)))
    (if (= (gettime! out) 0)
        out
        (error "clock_gettime() failed?"))))

(: unix->tai64n (integer -> u8vector))
(define (unix->tai64n secs)
  (let* ((out     (make-u8vector 12))
         (encode! (foreign-lambda void "tai64n_encode" integer64 integer32 u8vector))
         (+leap   (foreign-lambda integer64 "addleap" integer64)))
    (encode! (+ (+leap secs) (expt 2 62)) 0 out)
    out))

(: decode-int (u8vector fixnum fixnum --> integer))
(define (decode-int vec from to)
  (let loop ((i from)
             (v 0))
    (if (= i to)
        v
        (loop (+ i 1) (+
                       (arithmetic-shift v 8)
                       (u8vector-ref vec i))))))

;; tai64n-seconds yields the seconds component
;; of the tai64n object
(: tai64n-seconds (u8vector --> integer))
(define (tai64n-seconds tm)
  (decode-int tm 0 8))

;; tai64n-nanoseconds yields the
;; nanoseconds component of the tai64n object
(: tai64n-nanoseconds (u8vector --> integer))
(define (tai64n-nanoseconds tm)
  (decode-int tm 8 12))

(: tai64n->integer (u8vector --> integer))
(define (tai64n->integer tm)
  (+ (* (tai64n-seconds tm) 1000000000)
     (tai64n-nanoseconds tm)))

(: tai64n->unix (u8vector -> integer))
(define (tai64n->unix tm)
  (let* ((secs  (tai64n-seconds tm))
         (-leap (foreign-lambda integer64 "subleap" integer64)))
    (-leap secs)))

(: tai64n->string (u8vector --> string))
(define (tai64n->string n)
  (let* ((alph "0123456789abcdef")
         (out  (make-string 24)))
    (let loop ((i 0)
               (d 0))
      (if (= i 12)
          out
          (let ((byte (u8vector-ref n i)))
            (string-set! out d (string-ref alph (arithmetic-shift byte -4)))
            (string-set! out (+ d 1) (string-ref alph (bitwise-and byte #xf)))
            (loop (+ i 1) (+ d 2)))))))

(: string->tai64n (string --> (or false u8vector)))
(define (string->tai64n str)
  (and (= (string-length str) 24)
       (let ((out     (make-u8vector 12))
             (secs    (string->number (##sys#substring str 0 16) 16))
             (nsec    (string->number (##sys#substring str 16 24) 16))
             (encode! (foreign-lambda void "tai64n_encode" integer64 integer32 u8vector)))
         (encode! secs nsec out)
         out)))
