(cond-expand
  (csi (import r7rs)
       (load "execline.sld"))
  (else (begin)))

(import
  (chicken port)
  (distill execline))

(define (exexpr->string expr)
  (with-output-to-string
    (lambda () (write-exexpr expr))))

(define conf "/etc/sysctl.conf")

(let ((got (exexpr->string
	     (exec-begin
	       (make -j4)
	       (make install))))
      (want #<<EOF
#!/bin/execlineb -P
if {
	make -j4
}
make install

EOF
))
  (unless (equal? got want)
    (display "got:\n")
    (display got)
    (display "\nwant:\n")
    (display want)
    (newline)))

(define
  script
  (execline*
    ;; test string quoting when spaces are present
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
