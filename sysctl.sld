(define-library (distill sysctl)
  (export
    sysctls->string
    sysctl-service
    default-sysctls)
  (import
    scheme
    (distill plan)
    (distill service))
  (cond-expand
    (chicken (import
	       (only (chicken port) with-output-to-string)
               (only (chicken base) include error))))
  (include "sysctl.scm"))
