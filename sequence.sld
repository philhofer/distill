(define-library (distill sequence)
  (export
    list->seq
    seq->list
    vector->seq
    seq->vector
    proc->seq
    string-sep->seq
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
    any/s?
    all/s?
    for-each/s)
  (import
    scheme
    (scheme base)
    (srfi 69)
    (only (srfi 13) substring/shared))
  (cond-expand
    (chicken (import
               (chicken type)
               (only (chicken port) call-with-output-string)
               (only (chicken base) void))))
  (include "sequence.scm"))

