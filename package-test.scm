(import
 scheme
 (distill kvector)
 (distill package))

(define ($vars conf)
  (kvector*
   ARCH: ($arch conf)
   TARGET: ($triple conf)))

(define ($myflags conf)
  (list '-x '-y))

;; test config substitution
(let* ((conf (make-config arch: 'armv7 triple: 'arm-none-eabi))
       (subs (lambda (expr) ((vargs expr) conf))))
  (test equal?
	;; test sublist concatenation
	'(armv7 --foo "-march=armv7" "--target=arm-none-eabi" "K=V" "--libdir=/sysroot/arm-none-eabi/usr/lib")
	(subs `(,$arch --foo (-march= ,$arch) (--target= ,$triple) "K=V" (--libdir= ,$sysroot "/usr/lib"))))
  (test equal?
	;; test automatic splicing
	'(--flag "ARCH=armv7" "TARGET=arm-none-eabi" -x -y --another)
	(subs `(--flag ,$vars ,$myflags --another))))

;; test cmd*
(let* ((conf (make-config arch: 'x86_64 triple: 'x86_64-linux-musl))
       (expr (lambda (v) (v conf))))
  (test equal?
	'((if ((./configure "--target=x86_64-linux-musl")))
	  (make "CC=x86_64-linux-musl-gcc" install))
	(expr
	 (cmd*
	  `(./configure (--target= ,$triple))
	  `(make (CC= ,$triple -gcc) install))))
  (test equal?
	'((if ((./configure "ARCH=x86_64" "TARGET=x86_64-linux-musl" --disable-nls)))
	  (make install))
	(expr
	 (cmd*
	  `(./configure ,$vars --disable-nls)
	  '(make install))))
  ;; test list-of-list terminal form
  (test equal?
	'((if ((make install)))
	  (if ((cd foo/)
	       (make foo)))
	  (redirfd -r 0 busybox.links)
	  (forstdin -o 0 link)
	  (importas "-i" -u link link)
	  (ln -s /bin/busybox "/out/${link}"))
	(expr
	 (cmd*
	  '(make install)
	  '((cd foo/) (make foo))
	  '((redirfd -r 0 busybox.links)
	    (forstdin -o 0 link)
	    (importas "-i" -u link link)
	    (ln -s /bin/busybox "/out/${link}")))))
  (test equal?
	(cons
	 '(if ((make "CC=x86_64-linux-musl-gcc" install)))
	 (strip-binaries-script ($triple conf)))
	(expr
	 (cmd*
	  `(make (CC= ,$triple -gcc) install)
	  $strip-cmd))))
  
