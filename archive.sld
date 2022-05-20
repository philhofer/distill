(define-library (distill archive)
  (export
   dir->tar.zst
   fork+dir->tar.zst)
  (import
   scheme
   (srfi 11) ;; let-values
   (distill eprint)
   (distill coroutine))
  (cond-expand
    (chicken
      (import
        (chicken type)
        (chicken foreign)
        (chicken file posix)
        (chicken process)
        (only (chicken file) create-temporary-file)
        (only (chicken io) read-string)
        (only (chicken base) include))))
  (include "archive.scm"))
