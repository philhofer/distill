(import
 scheme
 (chicken file)
 (only (chicken io) read-string)
 (distill hash)
 (distill filepath)
 (distill plan))

(include "test-helpers.scm")

(artifact-dir "./test-artifacts")
(plan-dir "./test-plans")

(define build
  (interned
   "/build" #o755
   (lambda ()
     (display "#!/bin/sh -e")
     (newline)
     (display "echo OK! > /out/script-out")
     (newline))))

(let ((text "#!/bin/sh -e\necho OK! > /out/script-out\n")
      (path (filepath-join (artifact-dir) (artifact-hash build))))
  (test (hash-of text)
        (artifact-hash build))
  (test path (file-exists? path))
  (test text (with-input-from-file path read-string)))

(let* ((foo  (interned
              "/etc/foo" #o755 "file is foo"))
       (frob (interned
              "/etc/frob" #o755 "file is frob"))
       (pln (make-plan
             name: "test-match-plan"
             null-build: #t
             inputs: (list
                      (make-input
                       basedir: "/out"
                       link: foo)
                      (make-input
                       basedir: "/out"
                       link: frob)))))
  (build-graph! (list pln))
  (let ((art (plan-outputs pln)))
    (let* ((globs '("./etc/f*"))
           (deriv (make-plan
                   name: "test-deriv"
                   null-build: #t
                   inputs: (list
                            (make-input
                             basedir: "/out"
                             link:    pln
                             wrap:    (lambda (art)
                                        (sub-archive art globs))))))
           (art2  (plan-outputs (begin (build-graph! (list deriv)) deriv))))
      (test string=?
            (artifact-hash art)
            (artifact-hash art2)))))
