(import
  scheme
  (chicken time)
  (distill tai64))

(include "test-helpers.scm")

;; hopefully allowing a 1-second grace period
;; will keep this test from being flaky...
(let* ((billion     (expt 10 9))
       (close?      (lambda (a b) (<= (abs (- a b)) billion)))
       (sec         (current-seconds))
       (now         (tai64n-now)))
  (test = (string->tai64n (tai64n->string now)) now)
  (test close? (unix->tai64n sec) now))
