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
        (chicken file)
        (chicken string)
        (chicken file posix)
        (chicken process-context)
        (chicken type))))
  (include "filepath.scm"))
