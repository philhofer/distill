(define-library (distill filepath)
  (export
    string-prefix?
    string-suffix?
    filepath-join
    dirname
    basename
    abspath)
  (import
    scheme
    (srfi 2)
    (srfi 26)) ;; cut
  (cond-expand
    (chicken
      (import
        (only (chicken base) include error)
	(only (chicken string) substring=?)
        (only (chicken process-context) current-directory)
        (chicken type))))
  (include "filepath.scm"))
