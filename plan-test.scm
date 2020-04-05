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

(display "plan artifact test OK.\n")
