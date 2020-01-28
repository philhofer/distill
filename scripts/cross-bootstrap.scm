(import
  (chicken process-context)
  (chicken string)
  (package)
  (eprint)
  (base))

;; this script builds prebuilt bootstrap binaries
;; for a specific architecture

(define arch
  (let ((args (command-line-arguments)))
    (if (null? args)
      (fatal "usage:" (program-name) "<arch>")
      (string->symbol (car args)))))

(define build!
  (config->builder `((arch . ,arch)
                     (CFLAGS . (-pipe -fstack-protector-strong -Os)))))

(let ((alist (map cons
                  '(make execline busybox binutils gcc)
                  (build! make execline-tools busybox-core native-binutils native-gcc))))
  (with-output-to-file
    (conc "prebuilt-"  arch ".scm")
    (lambda ()
      (write (list 'quote alist)))))
