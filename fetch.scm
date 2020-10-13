;; %fetch stores a promise that lazily evaluates
;; to a function for downloading a file
(define %fetch
  (let ((run (lambda (proc . args)
               (receive (pid ok? status) (process-wait/yield (process-run proc args))
                 (and ok? (= status 0))))))
    (delay
      (cond
       ;; prefer wget, since it is often present
       ;; even on ultra-slim busybox systems
       ((pathfind "wget") =>
        (lambda (prog)
          (lambda (url dst)
            (run prog "-q" "-O" dst url))))
       ((pathfind "curl") =>
        (lambda (prog)
          (lambda (url dst)
            (run prog "-s" "-S" "-o" dst url))))
       (else (fatal "neither curl(1) nor wget(1) in $PATH"))))))

;; from the given url and puts it in the given file;
;; presently this is implemented by exec-ing wget
;; or curl depending on which one happens to be
;; available
(: fetch (string string -> *))
(define (fetch url dst)
  ;; both wget and curl interpret '-' as stdout,
  ;; so avoid that case in this circumstance
  (if (string=? dst "-")
      (error "refusing to fetch to stdout")
      ((force %fetch) url dst)))

;; fetch+hash performs (fetch url dst) and
;; returns the hash of 'dst'
;;
;; TODO: optimize this; we could hash the contents
;; of the data while it is being downloaded, and
;; with a hash function like BLAKE3 we could perform
;; incremental validation...
(: fetch+hash (string string -> (or string false)))
(define (fetch+hash url dst)
  (and (fetch url dst)
       (hash-file dst)))
