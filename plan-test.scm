(import
  (chicken file)
  (chicken pretty-print)
  (table)
  (execline)
  (log)
  (hash)
  (plan))

(include "test-helpers.scm")

(artifact-dir "./test-artifacts")
(plan-dir "./test-plans")
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
    (local-archive 'tar.zst (hash-file "bootstrap-x86_64.tar.zst")))

  ;; run a trivial plan ("echo hi > /sysroot/out") in
  ;; the bootstrap leaf and ensure that the output is
  ;; a valid interned file in the right place
  (let ((p (lambda (conf)
	     (make-package
	       #:label  "test-package"
	       #:src    bootstrap-input
	       #:inputs '()
	       #:tools  '()
	       #:build (make-recipe
			  #:script (execline*
				     (redirfd -w 1 /out/out)
				     (echo hi there!))))))
	(conf (table->proc (table))))
    (let ((out (build-package! p conf conf)))
      (pp out))))

