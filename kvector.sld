(define-library (distill kvector)
  (export
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
    kvector-setter)
  (import
    scheme
    (scheme base)
    (srfi 69))
  (cond-expand
    (chicken (import
               (chicken fixnum)
               (only (chicken keyword) keyword?)
               (chicken sort)
               (chicken type))))
  (include "kvector.scm"))
