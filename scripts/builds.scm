(import
  (distill plan)
  (distill hash)
  (distill filepath)
  (chicken process-context)
  (chicken file)
  (chicken io))

;; print the contents of the plan directory as columnar data:
;;   <plan-short-hash> <plan-label> <output-short-hash|"none">
;;
;; note that the output is unsorted
(let ((file->datum (lambda (f)
                     (with-input-from-file f read)))
      (file->text  (lambda (f)
                     (with-input-from-file f read-string))))
  (find-files
    (plan-dir)
    #:test ".*/label$"
    #:seed (void)
    #:dotfiles #f
    #:action (lambda (f state)
               (let* ((name  (file->text f))
                      (phash (basename
			      (##sys#substring f 0 (- (string-length f)
						      (string-length "/label")))))
                      (ofile (filepath-join
                               (plan-dir) phash "outputs.scm"))
                      (odata (if (file-exists? ofile)
                               (short-hash (artifact-hash (file->datum ofile)))
                               "none")))
                 (display (short-hash phash))
                 (display " ")
                 (display name)
                 (display " ")
                 (display odata)
                 (newline)))))

