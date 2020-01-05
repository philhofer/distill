(cond-expand
  (csi (import r7rs)
       (load "execline.sld"))
  (else (begin)))

(import
  (execline)
  fmt)

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
    (if ((sysctl -p ,conf)))
    (forbacktickx file ((pipeline ((elglob extra /etc/sysctl.d/*.conf)
				   (echo $extra)))
			(sort)))
    (importas file f)
    (echo $f)))

(define
  want
  #<<EOF
#!/bin/execlineb -P
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

(let ((got (reverse (execline-execs script)))
      (want '(if sysctl forbacktickx pipeline elglob echo sort importas echo)))
  (when (not (equal? got want))
    (display "---- exexpr-fold got: ----\n")
    (display got)
    (display "\n---- exexpr-fold want: ----\n")
    (display want)
    (error "fold results not equal")))
