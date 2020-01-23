(import
  (hash)
  (eprint)
  (chicken process-context))
(for-each
  (lambda (f)
    (let ((h (hash-file f)))
      (if h
        (begin
          (display h)
          (newline))
        (fatal "file doesn't exist:" f))))
  (let ((args (command-line-arguments)))
    (if (or (null? args)
            (and (string=? (car args) "-")
                 (null? (cdr args))))
      (list "/proc/self/fd/0")
      args)))
