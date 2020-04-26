(define-library (distill sequence)
  (export
    list->seq
    seq->list
    vector->seq
    seq->vector
    proc->seq
    string-sep->seq
    s/bind
    s/filter
    k/filter
    s/map
    k/map
    k/hash-ref
    s/uniq
    k/uniq
    s/append
    k/recur
    s/cons*
    k/preorder
    k/postorder
    kompose
    empty-seq
    lines/s
    join/s
    any/s?
    all/s?
    for-each/s
    ->lines+spaces)
  (import
    scheme
    (srfi 69))
  (cond-expand
    (chicken (import
               (chicken type)
               (only (chicken port) with-output-to-string call-with-output-string)
               (only (chicken base) void include error))))
  (include "sequence.scm"))

