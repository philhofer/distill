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
    (scheme base)
    (distill plan)
    (distill memo)
    (distill base)
    (distill package)
    (distill buildenv)
    (distill service)
    (distill sequence)
    (distill linux)
    (distill execline))
  (cond-expand
    (chicken (import
               (only (chicken string) conc)
               (typed-records))))
  (include "net.scm"))
