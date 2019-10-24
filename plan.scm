
;; artifact: a set of files
(define-record-type
  <artifact>
  (raw-artifact name type hash plan url)
  artifact?
  (name art-name art-name-set!) ;; human-readable name
  (type art-type art-type-set!) ;; human-readable type
  (hash art-hash art-hash-set!) ;; content-addressed name, or #f if unknown
  (plan art-plan art-plan-set!) ;; the plan that produces this artifact, or #f if unknown
  (url  art-url  art-url-set!)) ;; a url (string) that provides a previous invocation of this artifact's plan

;; plan: a set of steps to perform on a set of inputs
(define-record-type
  <plan>
  (raw-plan name steps inputs)
  plan?
  (name   plan-name   plan-name-set!)    ;; human-readable name
  (steps  plan-steps  plan-steps-set!)   ;; list of serializeable steps in the plan (TODO: representation?)
  (inputs plan-inputs plan-inputs-set!)) ;; list of inputs (can be empty)

;; safe-artifact? returns whether or not the
;; specification of this artifact is "safe"
;;
;; artifacts must either have a plan, or
;; a url *with* a hash (some can have both)
(define (safe-artifact? art)
  (or (art-plan art)
      (and (art-hash art)
	   (art-url art))))

;; not defined in R7RS, but trivial enough to implement
(define (call-with-output-string proc)
  (let ((p (open-output-string)))
    (proc p)
    (let ((s (get-output-string p)))
      (close-output-port p)
      s)))

(define (hash? x)
  (and (bytevector? x)
       (= (bytevector-length x) 32)))

(define (foldl1 proc seed lst)
  (let loop ((state seed)
	     (lst   lst))
    (if (null? lst)
	state
	(loop (proc state (car lst)) (cdr lst)))))

;; plan-hash returns the canonical hash for the plan
;;
;; if any of the plan input artifacts don't have
;; a hash, plan-hash returns #f
(define (plan-hash p)
  ;; TODO: this only works consistently if each
  ;; scheme implementation outputs exactly the
  ;; same things for "write;" this may not be
  ;; true in practice...
  (and (let loop ((in (plan-inputs p)))
	 (or (null? in)
	     (and (art-hash (car in))
		  (loop (cdr in)))))
       (hash-string
	 (call-with-output-string
	   (lambda (out)
	     (for-each
	       (lambda (inpt)
		 (write (art-hash inpt) out)
		 (newline out))
	       (plan-inputs p))
	     (for-each
	       (lambda (step)
		 (write step out)
		 (newline out))
	       (plan-steps p)))))))

;; remote-artifact creates an artifact
;; that is always downloaded from a remote source
(define (remote-artifact name hash url)
  ;; remote artifacts don't have a plan, but
  ;; always have a url
  (raw-artifact name 'remote hash #f url))

;; generic DFS DAG-walking procedure
;;
;; call (proc node) on each node, and use
;; (get-edges node) to get a list of edges
;;
;; errors on cycles
(define (for-each-dag-dfs root proc get-edges)
  (define tbl (make-hash-table))
  (define (walked? x)
    (let ((c (hash-table-ref/default tbl x 0)))
      (when (= c 1)
	(error "cycle: " x))
      (not (= c 0))))
  (define (walk x)
    (unless (walked? x)
      (begin
	(hash-table-set! tbl x 1)
	(for-each walk (get-edges x))
	(hash-table-set! tbl x 2)
	(proc x))))
  (walk root))

(define (artifacts-dfs proc root)
  (for-each-dag-dfs root proc (lambda (x)
			    (let ((p (art-plan x)))
			      (if p (plan-inputs p) '())))))

;; artifacts-load-hashes! walks an artifact DAG
;; and restores known plan-hash : artifact-hash
;; relationships
(define (reload-art-hashes! root ht)
  (artifacts-dfs
    (lambda (a)
      (and-let* ((_  (not (art-hash a)))
		 (p  (art-plan a))
		 (ph (plan-hash p))
		 (ah (hash-table-ref/default ht ph #f)))
	(art-hash-set! a ah)))
    root))

;; artifacts-plan-hashes yields a hash table that can
;; be serialized and passed to artifacts-load-hashes!
;; in order to restore saved artifact:plan hash relationships
(define (save-art-hashes root)
  (let ((ht (make-hash-table)))
    (artifacts-dfs
      (lambda (a)
	(and-let* ((p  (art-plan a))
		   (ph (plan-hash p))
		   (ah (art-hash a)))
	  (hash-table-set! ht ah ph)))
      root)))

;; fetch-artifact->file fetches a remote artifact
;; by exec-ing 'curl <url> -o outfile'
#;(define (fetch-artifact->file art outfile)
  (and-let* ((url (art-url art)))
    (run curl ,url -o ,outfile)))
