(define-library (filepath)
  (export
    filepath-join
    dirname
    basename
    abspath)
  (import
    scheme
    (scheme base)
    (srfi 13))
  (cond-expand
    (chicken
      (import
        (only (chicken process-context) current-directory)
        (chicken type))))
  (include "filepath.scm"))
