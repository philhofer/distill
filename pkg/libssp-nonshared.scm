;; produce a libssp_nonshared independent of gcc to break a dependency cycle
(define libssp-nonshared
  (package-lambda
    conf
    (make-package
	#:label   (conc "libssp-nonshared-" (conf 'arch))
	#:src     '()
	#:tools   (cc-for-target conf)
	#:inputs  '()
	#:overlay (interned "/src/ssp-nonshared.c" #o644 #<<EOF
extern void __stack_chk_fail(void);
void __attribute__((visibility ("hidden"))) __stack_chk_fail_local(void) { __stack_chk_fail(); }

EOF
)
	#:build (make-recipe
		  #:script (let* ((cenv (cc-env conf))
				  (CC   (cdr (assq 'CC cenv)))
				  (AR   (cdr (assq 'AR cenv))))
			     (execline*
			       (cd ./src)
			       (if ((,CC -c -fPIE -Os ssp-nonshared.c -o __stack_chk_fail_local.o)))
			       (if ((,AR -Dcr libssp_nonshared.a __stack_chk_fail_local.o)))
			       (if ((mkdir -p /out/usr/lib)))
			       (cp libssp_nonshared.a /out/usr/lib/libssp_nonshared.a)))))))
