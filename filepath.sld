(define-library (filepath)
  (export
    foldl-dir
    filepath-join
    abspath)
  (import
    scheme
    (srfi 13)
    (chicken file)
    (chicken string)
    (chicken file posix)
    (chicken process-context)
    (chicken type))
  (include "filepath.scm"))
