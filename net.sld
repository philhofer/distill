(define-library (distill net)
  (export
    ;; services:
    netif-loopback

    ;; packages:
    libmnl
    libnftnl
    iptables
    iproute2)
  (import
    scheme
    (distill plan)
    (distill memo)
    (distill base)
    (distill package)
    (distill kvector)
    (distill service)
    (distill sequence)
    (distill linux)
    (distill execline))
  (cond-expand
    (chicken (import
               (only (chicken base) include)
               (only (chicken string) conc))))
  (include "net.scm"))
