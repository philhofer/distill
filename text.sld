(define-library (distill text)
  (export
   lines
   map-lines
   join-with)
  (import
    scheme
    (only (chicken port) with-output-to-string)
    (only (chicken base) include intersperse identity))
  (include "text.scm"))
