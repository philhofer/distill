(define-library (distill net)
  (export
    ;; services:
    netif-loopback)
  (import
    scheme
    (distill base)
    (distill kvector)
    (distill service))
  (cond-expand
    (chicken (import
               (only (chicken base) include)
               (only (chicken string) conc))))
  (include "net.scm"))
