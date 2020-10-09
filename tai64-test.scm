(import
 scheme
 (chicken time)
 (distill tai64))

(include "test-helpers.scm")

;; hopefully allowing a 1-second grace period
;; will keep this test from being flaky...
(let* ((close?      (lambda (a b) (<= (abs (- a b)) 1)))
       (sec         (current-seconds))
       (now         (tai64n-now)))
  (test equal? (string->tai64n (tai64n->string now)) now)
  (test close? sec (tai64n->unix now)))

(define (tai64-unix-secs tm)
  (- (tai64n-seconds tm) (expt 2 62)))

(let ((unix->tai-string (lambda (x)
                          (tai64n->string (unix->tai64n x)))))
  (test* string=? unix->tai-string (("400000000000000000000000" 0)
                                    ("400000000000000100000000" 1)
                                    ("400000005e8fa52c00000000" 1586472199)
                                    ("4000000000ffffff00000000" #xffffff))))
