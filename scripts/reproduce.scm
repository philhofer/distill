(import
  scheme
  (distill plan)
  (distill eprint)
  (chicken process-context)
  (chicken port))

(let ((args (command-line-arguments)))
  (when (null? args)
    (fatal "usage:" (program-name) "<plan-hash>"))
  (let* ((hash (car args))
         (plan (load-plan hash))
         (old  (plan-outputs plan))
         (out  (build-plan! plan)))
    (if old
      (unless (string=?
                (artifact-hash old)
                (artifact-hash out))
        (fatal "uh oh; got a new build hash:" (artifact-hash out)))
      (info "new (unsaved) output:" (artifact-hash out)))))
