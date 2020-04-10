(define-library (distill tai64)
  ;; TAI is the current international real-time standard.
  ;; A TAI64 label is a 64-bit integer corresponding
  ;; to 2^62 plus the number of seconds elapsed since
  ;; 1970 TAI (in other words, seconds since the unix
  ;; epoch displaced by 2^62, so unix time 0 is TAI64 2^62).
  ;; A TAI64N label is a TAI64 label plus a 32-bit integer
  ;; containing additional nanosecond precision.
  ;; By convention, TAI64{N,NA} labels are printed
  ;; as (big-endian) hex strings.
  ;;
  ;; for example:
  ;;   (tai64n->string (tai64n-now)) ;; => "400000005e8fa9b235cf61eb"
  (export
    tai64n-now
    unix->tai64n
    tai64n->unix
    tai64n-seconds
    tai64n-nanoseconds
    string->tai64n
    tai64n->string)
  (import
    scheme
    (srfi 4)
    (srfi 11)
    (srfi 13))
  (cond-expand
    (chicken (import
               (only (chicken base) include error)
               (chicken bitwise)
               (chicken type)
               (chicken foreign))))
  (include "tai64.scm"))
