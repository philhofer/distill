(define-library (distill system)
  (export
    platform?
    make-platform
    platform-mkimage
    platform-kernel
    platform-cmdline
    platform-config
    platform-services
    platform-packages
    system?
    make-system
    system-services
    system-packages

    platform+system->plan)
  (import
    scheme
    (distill kvector)
    (distill contract)
    (distill unix)
    (only (distill package) config? expander)
    (only (distill service) service? services->packages)
    (only (distill plan) artifact?))
  (cond-expand
    (chicken (import
               (only (chicken base) unless include disjoin error))))
  (include "system.scm"))
