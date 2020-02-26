(include "kvector.mod.scm")

(import
  (chicken condition)
  (distill kvector))

(include "test-helpers.scm")

(define ktd (make-kvector-type
              first:
              second:
              third:
              fourth:
              fifth:))

(define my-kv? (kvector-predicate ktd))
(define make-kv (kvector-constructor ktd))

(define third (kvector-getter ktd third:))
(define third-set! (kvector-setter ktd third:))

(let* ((args '(first:  0
               second: 1
               third:  "hello"
               fourth: "world"
               fifth:  #f))
       (kv   (apply make-kv args))
       (kv2  (list->kvector args)))
  (test eq? #t  ((keys/c first: third: fifth:) kv))
  (test eq? #t  ((keys/c second: fourth:) kv))
  (test eq? #f  ((keys/c name:) kv))
  (test eq? #t  (my-kv? kv))
  (test "hello" (third kv))
  (test equal?  kv kv2)
  (test eq?     (ktype kv) (ktype kv2))
  (test eq? #f  ((kvector-getter (ktype kv2) fifth:)
                 kv2))
  (test equal?
        (kvector->list kv)
        (kvector->list kv2))
  (test* kref   ((0 kv first:)
                 (1 kv second:)
                 ("hello" kv third:)
                 ("world" kv fourth:)
                 (#f kv fifth:)))
  (third-set! kv "HELLO")
  (test string=? "HELLO" (third kv)))

(define ktd2 (make-kvector-type
               string:
               symbol:
               list:))

(define (throws? thunk)
  (call/cc
    (lambda (ret)
      (parameterize ((current-exception-handler (lambda (exn)
                                                  (ret #t))))
        (thunk)
        #f))))

(define make-kv2 (kvector-contract-constructor
                   ktd2
                   string: string?
                   symbol: symbol?
                   list:   list?))

(test eq? #t ((kvector-predicate ktd2)
              (make-kv2
                string: "yes"
                symbol: 'yes
                list:   '(x y))))

(test eq? #t (throws?
               (lambda ()
                 (make-kv2
                   string: #f
                   symbol: 'yes
                   list:   '(x y z)))))
(test eq? #t (throws?
               (lambda ()
                 (make-kv2
                   string: "yes"
                   symbol: "no"
                   list:   '()))))

(let ((similar (kvector*
                 string: "yes"
                 symbol: 'foo
                 list:   '(x y z)
                 extra:  'bar)))
  (test equal?
        (make-kv2
          string: "yes"
          symbol: 'foo
          list:   '(x y z))
        (recast ktd2 similar)))

(let ((default (make-kv2
                 string: "default"
                 symbol: 'foo
                 list:   '()))
      (value   ((kvector-constructor ktd2)
                 string: "value")))
  (test equal?
        (make-kv2
          string: "value"
          symbol: 'foo
          list:   '())
        (kvector-union! value default)))
