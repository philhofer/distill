(include "contract.mod.scm")

(import
  scheme
  (distill contract))

(include "test-helpers.scm")

(define-syntax yes
  (syntax-rules ()
    ((_ args* ...)
     (begin
       (test eq? #t args*) ...))))

(define-syntax no
  (syntax-rules ()
    ((_ args* ...)
     (begin
       (test eq? #f args*) ...))))

(yes
  ((vector/c string? symbol? list?)
   (vector "foo" 'foo '()))
  ((list/c string? symbol? list?)
   (list "foo" 'foo '(x y z)))
  ((and/c integer? number?) 3)
  ((or/c symbol? number?) 3.5)
  ((or/c symbol? number?) 'foo)
  ((or/c (eq?/c 'foo) (eq?/c 'bar)) 'foo)
  ((or/c (eq?/c 'foo) (eq?/c 'bar)) 'bar)
  ((pair-of (eq?/c 'foo) (eq?/c 'bar)) (cons 'foo 'bar)))

(no
  ((vector/c string? symbol? list?)
   (vector "foo" 'foo))
  ((vector/c string? symbol? list?)
   (vector 'foo '()))
  ((list/c string? symbol? list?)
   (list "foo" 'foo '() 'extra))
  ((and/c integer? number?) 3.5)
  ((or/c symbol? integer?) "nope")
  ((pair-of symbol? integer?) (cons 'yes 3.14159)))
