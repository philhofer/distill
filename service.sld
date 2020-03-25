(define-library (distill service)
  (export
    s6
    s6-rc
    hard
    busybox-full
    services->packages
    named*
    make-service
    update-service
    longrun*
    longrun?
    oneshot*
    oneshot?
    bundle*
    bundle?
    service-name
    service-spec
    service-after
    service-inputs)
  (import
    scheme
    (srfi 26)
    (srfi 69)
    (srfi 88)
    (distill plan)
    (distill kvector)
    (distill contract)
    (distill eprint)
    (distill package)
    (distill base)
    (distill unix)
    (distill linux)
    (distill sequence)
    (distill execline)
    (distill filepath))
  (cond-expand
    (chicken (import
               (chicken type)
               (only (chicken base) include error)
               (only (chicken irregex) glob->sre irregex-match?)
               (only (chicken string) conc)
               (only (chicken port) with-output-to-string))))
  (include "service.scm"))
