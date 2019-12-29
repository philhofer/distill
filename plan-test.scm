#!/bin/csi -s
(import
  (chicken file)
  (execline)
  (log)
  (hash)
  (plan))

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
	       (v  (cdr p))
	       (v0 (lookup tab k)))
	  ;; values are never copied when inserted
	  ;; into the table, so the must be 'eq?' to
	  ;; their inputs
	  (unless (eq? v v0)
	    (error "for key" k "want" v "got" v0))))
      data)
    (when (lookup tab 'not-a-key)
      (error "erroneous lookup succeeded?"))))

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
    (let ((out  (plan-outputs conf p))
	  (want `(#("/out" ,(hash-string "hi there!\n") #o644))))
      (unless (equal? out want)
	(error "not equal:"
	       out want)))))

