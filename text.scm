(define (map-lines proc lst)
  (let ((dsp (lambda (line)
		    (if (list? line)
			(for-each display (intersperse line " "))
			(display line))
		    (newline))))
    (with-output-to-string
      (lambda ()
	(for-each
	 (lambda (item) (dsp (proc item)))
	 lst)))))

(define (join-with sep lst)
  (with-output-to-string
    (lambda ()
      (for-each display (intersperse lst sep)))))

(define (lines lst) (map-lines identity lst))
