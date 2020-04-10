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

(define build! (config->builder (default-config arch)))

(let ((alist (map cons
                  '(make execline busybox binutils gcc)
                  (build! make execline-tools busybox-core native-binutils native-gcc))))
  (with-output-to-file
    (conc "prebuilt-"  arch ".scm")
    (lambda ()
      (write (list 'quote alist)))))
(exit 0)
