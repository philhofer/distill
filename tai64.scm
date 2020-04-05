(foreign-declare "#include <time.h>")

(define billion (expt 10 9))

;; currently, TAI64 time is 37 seconds ahead of UTC
;; TODO: actually use a leap-second table so that
;; results between 1972 and 2016 are accurate to 1s;
;; there were 37 leap-seconds in that range
(define tai64-epoch (+ (expt 2 62) 37))

(: unix->tai64n (integer --> integer))
(define (unix->tai64n secs)
  (* billion (+ secs tai64-epoch)))

(: tai64n->unix (integer --> integer))
(define (tai64n->unix tai)
  (- (quotient tai billion) tai64-epoch))

(: tai64n-now (-> integer))
(define (tai64n-now)
  (let ((out      (make-s64vector 2))
        (gettime! (foreign-lambda* int ((s64vector mem)) #<<EOF
struct timespec ts;
int err = clock_gettime(CLOCK_REALTIME, &ts);
if (err == 0) {
    mem[0] = (int64_t)ts.tv_sec;
    mem[1] = (int64_t)ts.tv_nsec;
}
C_return(err);
EOF
)))
        (or (= (gettime! out) 0)
            (error "clock_gettime() failed?"))
        (+ (unix->tai64n (s64vector-ref out 0))
           (s64vector-ref out 1))))

(: tai64n->string (integer --> string))
(define (tai64n->string n)
  (define (padl str len)
    (let ((pad  "00000000")
          (slen (string-length str)))
      (if (= slen len)
        str
        (string-append
          (substring/shared pad 0 (- len slen))
          str))))
  (let-values (((secs nsecs) (quotient&remainder n billion)))
    (string-append
      (padl (number->string secs 16) 16)
      (padl (number->string nsecs 16) 8))))

(: string->tai64n (string --> (or false integer)))
(define (string->tai64n str)
  (and (= (string-length str) 24)
       (+ (* billion (string->number (substring/shared str 0 16) 16))
          (string->number (substring/shared str 16 24) 16))))
