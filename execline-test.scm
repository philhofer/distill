(cond-expand
  (csi (include "execline.mod.scm")
       (include "kvector.mod.scm")
       (include "text.mod.scm"))
  (else (begin)))
(include "test-helpers.scm")

(import
  scheme
  (chicken port)
  (distill kvector)
  (distill execline))

(define (exexpr->string expr)
  (with-output-to-string
    (lambda () (write-exexpr expr))))

(define conf "/etc/sysctl.conf")

(define
  script
  `(if (sed -e s/foo/bar/g "s/foo = bar/bar = foo/g" file)
    ifelse (echo #u8(127 69 76 70)) (echo "echo failed")
    if (sysctl -p ,conf)
    ;; test (lack of) quoting for simple strings
    forbacktickx file (pipeline
		       (elglob extra "/etc/sysctl.d/*.conf" echo $extra)
		       sort)
    importas file f
    echo $f))

(define
  want
  #<<EOF
#!/bin/execlineb -P
if {
	sed -e s/foo/bar/g "s/foo = bar/bar = foo/g" file
}
ifelse {
	echo "\0x7fELF"
} {
	echo "echo failed"
}
if {
	sysctl -p /etc/sysctl.conf
}
forbacktickx file {
	pipeline {
		elglob extra /etc/sysctl.d/*.conf echo $extra
	} sort
}
importas file f echo $f

EOF
)

(define scriptout (exexpr->string script))
(when (not (equal? scriptout want))
  (display "---- script: ----\n")
  (display scriptout)
  (display "---- want: ----\n")
  (display want)
  (error "scripts not equal"))

(test equal?
      '(if (./configure x y z)
	   if (make args)
	   make install)
      (elif*
       '(./configure x y z)
       '(make args)
       '(make install)))

(test string=?
      "CFLAGS=-ffoo -fbar"
      ((el= 'CFLAGS= identity) '(-ffoo -fbar)))

;; simple template substitution test
(let* (($CC      identity)
       ($CFLAGS  (lambda (cc) '(-ffoo -fbar)))
       (template `(if (make ,(elconc 'CC= $CC)
			,(el= 'CFLAGS= $CFLAGS))
		      make install)))
  (test equal?
	'(if (make "CC=gcc" "CFLAGS=-ffoo -fbar")
	     make install)
	(elexpand "gcc" template)))

;; test optimization: sublists that are not
;; altered can be returned as themselves
(let* ((template '(make DESTDIR=/out install)))
  (test eq?
	template
	(elexpand #f template)))

;; test expansion within elif
(let* (($var      identity)
       ($subscript (lambda (conf)
		     (elexpand conf `(redirfd -r /dev/urandom if (head -c20) echo ,$var))))
       (topscript (elif `(echo ,$var) (list $subscript))))
  (test equal?
	'(if (echo "hi") redirfd -r /dev/urandom if (head -c20) echo "hi")
	(elexpand "hi" topscript)))
