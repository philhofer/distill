(define-library (distill sysctl)
  (export
    sysctl-service
    default-sysctls)
  (import
    scheme
    (scheme base)
    (distill plan)
    (distill service)
    (distill sequence))
  (include "sysctl.scm"))
