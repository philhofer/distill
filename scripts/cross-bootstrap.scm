(import
  (chicken process-context)
  (chicken string)
  (base))

;; this script builds prebuilt bootstrap binaries
;; for a specific architecture

(define arch
  (let ((args (command-line-arguments)))
    (if (null? args)
      (fatal "usage:" (program-name) "<arch>")
      (string->symbol (car args)))))

(define build!
  (config-builder `((arch . ,arch)
		    (CFLAGS . (-pipe -fstack-protector-strong -Os)))))

(let ((alist `((make .     ,(build! make))
	       (execline . ,(build! execline-tools))
	       (busybox .  ,(build! busybox-core))
	       (binutils . ,(build! native-binutils))
	       (gcc .      ,(build! native-gcc)))))
  (with-output-to-file
    (conc "prebuilt-"  arch ".scm")
    (lambda ()
      (write (cons 'quote alist)))))
