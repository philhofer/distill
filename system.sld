(define-library (distill system)
  (export
    build-system
    uniq-dd-script)
  (import
    scheme
    (distill execline)
    (distill filepath)
    (distill kvector)
    (distill contract)
    (distill service)
    (distill eprint)
    (distill plan))
  (cond-expand
    (chicken (import
               (only (chicken base) include)
               (only (chicken file posix) set-file-permissions!))))
  (include "system.scm"))
