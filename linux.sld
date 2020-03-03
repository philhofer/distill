(define-library (distill linux)
  (export
    perl
    libelf
    linux-headers
    linux/config-static
    linux-virt-x86_64)
  (import
    scheme
    (distill base)
    (distill plan)
    (only (distill memo) cons*)
    (distill package)
    (distill execline)
    (distill kvector)
    (distill filepath)
    (distill eprint)
    (only (distill image) xz-utils))
  (cond-expand
    (chicken (import
               (only (chicken base) include error unless)
               (only (chicken string) conc)
               (only (chicken port) with-output-to-string))))
  (include "linux.scm"))
