(define-library (distill kvector)
  (export
    kidx
    kref
    kref*
    kref/default
    kupdate
    ktype
    kvector?
    kvector*
    kvector->list
    kvector-foldl
    kvector-map
    list->kvector
    make-kvector-type
    recast
    kvector/c
    keys/c
    subvector-constructor
    kwith
    :=
    +=
    ?=
    kvector-union!
    kvector-constructor
    kvector-predicate
    kvector-getter
    kvector-setter
    define-kvector-type)
  (import
    scheme
    (srfi 69)
    (srfi 88))
  (cond-expand
    (chicken (import
               (only (chicken base) include error unless vector-copy!)
               (chicken fixnum)
               (chicken sort)
               (chicken type))))
  (include "kvector.scm"))
