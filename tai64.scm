(foreign-declare
#<<EOF
#include <time.h>
#include <stdint.h>
static const int64_t leapseconds[] = {
    63072000LL,
    78796800LL,
    94694400LL,
    126230400LL,
    157766400LL,
    189302400LL,
    220924800LL,
    252460800LL,
    283996800LL,
    315532800LL,
    362793600LL,
    394329600LL,
    425865600LL,
    489024000LL,
    567993600LL,
    631152000LL,
    662688000LL,
    709948800LL,
    741484800LL,
    773020800LL,
    820454400LL,
    867715200LL,
    915148800LL,
    1136073600LL,
    1230768000LL,
    1341100800LL,
    1435708800LL,
    1483228800LL,
};
#define LEAPS (sizeof(leapseconds)/sizeof(leapseconds[0]))

static int
subleap(int64_t sec)
{
    int i;

    for (i = LEAPS-1; i >= 0; i--) {
          if (sec >= leapseconds[i]+10LL+(long long)i) return sec - 10LL - (long long)i;
    }

    return sec;
}

static int
addleap(int64_t sec)
{
    int i;

    for (i = LEAPS-1; i >= 0; i--) {
        if (sec >= leapseconds[i]) return sec + 10LL + (long long)i;
    }
    return sec;
}

static void
tai64n_encode(int64_t sec, int32_t nsec, unsigned char *dst)
{
    dst[0] = ((sec >> 56) & 0xff);
    dst[1] = ((sec >> 48) & 0xff);
    dst[2] = ((sec >> 40) & 0xff);
    dst[3] = ((sec >> 32) & 0xff);
    dst[4] = ((sec >> 24) & 0xff);
    dst[5] = ((sec >> 16) & 0xff);
    dst[6] = ((sec >> 8)  & 0xff);
    dst[7] = ((sec >> 0)  & 0xff);
    dst[8] = ((nsec >> 24) & 0xff);
    dst[9] = ((nsec >> 16) & 0xff);
    dst[10] = ((nsec >> 8) & 0xff);
    dst[11] = ((nsec >> 0) & 0xff);
}

static int
tai64n_gettime(unsigned char *dst)
{
    struct timespec ts;
    int err;

    if ((err = clock_gettime(CLOCK_REALTIME, &ts)) < 0)
        return -errno;
    tai64n_encode(addleap(ts.tv_sec)+(1LL<<62), ts.tv_nsec, dst);
    return 0;
}

EOF
)

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

(: tai64n-seconds (u8vector --> integer))
(define (tai64n-seconds tm)
  (decode-int tm 0 8))

(: tai64n-nanoseconds (u8vector --> integer))
(define (tai64n-nanoseconds tm)
  (decode-int tm 8 12))

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
