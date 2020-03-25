(include "execline.mod.scm")
(include "test-helpers.scm")

(import
  scheme
  (chicken port)
  (distill execline))

(define (exexpr->string expr)
  (with-output-to-string
    (lambda () (write-exexpr expr))))

(define conf "/etc/sysctl.conf")

(define
  script
  `(;; test string quoting when spaces are present
    (if ((sed -e s/foo/bar/g "s/foo = bar/bar = foo/g" file)))
    (ifelse ((echo #u8(127 69 76 70)))
            ((echo "echo failed")))
    (if ((sysctl -p ,conf)))
    ;; test (lack of) quoting for simple strings
    (forbacktickx file ((pipeline ((elglob extra "/etc/sysctl.d/*.conf")
				   (echo $extra)))
			(sort)))
    (importas file f)
    (echo $f)))

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
		elglob extra /etc/sysctl.d/*.conf
		echo $extra
	}
	sort
}
importas file f
echo $f

EOF
)

(define scriptout (exexpr->string script))
(when (not (equal? scriptout want))
  (display "---- script: ----\n")
  (display scriptout)
  (display "---- want: ----\n")
  (display want)
  (error "scripts not equal"))
