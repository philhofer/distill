(define-library (distill sysctl)
  (export
    sysctls->string
    sysctl-service
    default-sysctls)
  (import
    scheme
    (distill plan)
    (distill service)
    (distill sequence))
  (cond-expand
    (chicken (import
               (only (chicken base) include error))))
  (include "sysctl.scm"))
