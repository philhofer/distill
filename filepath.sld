(define-library (distill filepath)
  (export
    filepath-join
    dirname
    basename
    abspath)
  (import
    scheme
    (scheme base)
    (srfi 2)
    (srfi 13)
    (srfi 26) ;; cut
    (distill sequence))
  (cond-expand
    (chicken
      (import
        (only (chicken process-context) current-directory)
        (chicken type))))
  (include "filepath.scm"))
