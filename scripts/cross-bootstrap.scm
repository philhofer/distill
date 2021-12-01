(import
  (chicken process-context)
  (chicken string)
  (distill package)
  (distill eprint)
  (distill base))

(define (cdn-url c-hash)
  (string-append
   "https://b2cdn.sunfi.sh/file/pub-cdn/"
   c-hash))

;; this script builds prebuilt bootstrap binaries
;; for a specific architecture

(define arch
  (let ((args (command-line-arguments)))
    (if (null? args)
      (fatal "usage:" (program-name) "<arch>")
      (string->symbol (car args)))))

(define conf (default-config arch))
(define build! (config->builder conf))

(let* ((alist (build! make exportall execline-tools busybox-core
                      (binutils-for-triple ($triple conf))
                      (gcc-for-triple ($triple conf))))
       (with-urls (map
                   (lambda (art)
                     (vector-set! art 2 (cdn-url (artifact-hash art)))
                     art))))
  (with-output-to-file (conc "prebuilt-"  arch ".scm")
    (lambda ()
      (write (list 'quote with-urls)))))
(exit 0)
