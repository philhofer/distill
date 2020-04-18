(import
  (chicken process-context)
  (chicken string)
  (distill package)
  (distill eprint)
  (distill base))

;; this script builds prebuilt bootstrap binaries
;; for a specific architecture

(define arch
  (let ((args (command-line-arguments)))
    (if (null? args)
      (fatal "usage:" (program-name) "<arch>")
      (string->symbol (car args)))))

(define conf (default-config arch))
(define build! (config->builder conf))

(let ((alist (map cons
                  '(make execline busybox binutils gcc)
                  (build! make execline-tools busybox-core
			  (binutils-for-triple ($triple conf))
			  (gcc-for-triple ($triple conf))))))
  (with-output-to-file
    (conc "prebuilt-"  arch ".scm")
    (lambda ()
      (write (list 'quote alist)))))
(exit 0)
