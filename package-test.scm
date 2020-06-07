(import
  scheme
  (distill package))

(include "test-helpers.scm")

(let* (($cc     identity)
       ($cflags (lambda (cc)
		  '(-O2 -fwrapv)))
       (expr    (list
		 `(CC . ,$cc)
		 `(CFLAGS . ,$cflags)
		 `(cmdline ,$cc ,$cflags))))
  (test equal?
	'(exportall
	  (cmdline "gcc -O2 -fwrapv"
		   CFLAGS "-O2 -fwrapv"
		   CC "gcc"))
	(exports->script "gcc" expr)))
