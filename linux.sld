(define-library (distill linux)
  (export
    perl
    linux-headers
    linux/config-static
    linux-virt-x86_64)
  (import
    scheme
    (scheme base)
    (distill base)
    (distill plan)
    (distill package)
    (distill buildenv)
    (distill execline)
    (distill filepath)
    (distill eprint)
    (only (distill image) xz-utils))
  (cond-expand
    (chicken (import
               (only (chicken string) conc)
               (only (chicken port) with-output-to-string))))
  (include "linux.scm"))
