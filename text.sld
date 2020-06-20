(define-library (distill text)
  (export
   lines
   map-lines
   join-with
   tabular)
  (import
    scheme
    (only (chicken port) with-output-to-string)
    (only (chicken base) include intersperse identity unless))
  (include "text.scm"))
