(import
  (chicken file)
  (execline)
  (log)
  (hash)
  (plan))

(include "test-helpers.scm")

(trace-log #t)

;; test table/lookup
(let ()
  (let* ((data '((foo . bar)
	 	 (four . 4)
		 (quux . "lorem ipsum dolor")
		 (baz list of symbols)
		 (longer-reference 1 2 3 4 5)
		 (alist (key . val) (key2 . val2))))
	 (tab  (table data)))
    (for-each
      (lambda (p)
	(let* ((k  (car p))
	       (v  (cdr p)))
	  (test eq? v (lookup tab k))))
      data)
    (test eq? #f (lookup tab 'not-a-key))))

;; test a simple plan execution
(let ()
  (define bootstrap-input
    (make-leaf
      #:name "bootstrap-x86_64"
      #:hash (hash-file "bootstrap-x86_64.tar.xz")
      #:format 'tar.xz))

  ;; run a trivial plan ("echo hi > /sysroot/out") in
  ;; the bootstrap leaf and ensure that the output is
  ;; a valid interned file in the right place
  (let ((p (make-plan
	     #:name "test-plan"
	     #:recipe (make-recipe
			#:script (execline*
				   (redirfd -w 1 /sysroot/out)
				   (echo hi there!)))
	     #:inputs (list bootstrap-input)))
	(conf (table->proc
		(table '((artifact-dir . "./test-artifacts")
			 (plan-dir     . "./test-plans"))))))
    (build-tree! conf p)
    (test
      `(#("/out" ,(hash-string "hi there!\n") #o644))
      (plan-outputs conf p))))

