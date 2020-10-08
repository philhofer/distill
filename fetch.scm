;; calling (fetch url dst) fetches the contents
;; from the given url and puts it in the given file;
;; presently this is implemented by exec-ing wget
;; or curl depending on which one happens to be
;; available
(: fetch (string string -> *))
(define fetch
  (let* ((run
          (lambda (proc . args)
            (receive (pid ok? status) (process-wait/yield (process-run proc args))
              (and ok? (= status 0)))))
         (pathfind
          (lambda (bin)
            (let loop ((lst (string-split (get-environment-variable "PATH") ":")))
              (and (pair? lst)
                   (or (file-exists? (filepath-join (car lst) bin))
                       (loop (cdr lst)))))))
         (%fetch
          (delay
            (cond
             ((pathfind "wget")
              (lambda (url dst)
                (run "wget" "-q" "-O" dst url)))
             ((pathfind "curl")
              (lambda (url dst)
                (run "curl" "-s" "-o" dst url)))
             (else (fatal "neither curl(1) nor wget(1) in $PATH"))))))
    (lambda (url dst)
      ((force %fetch) url dst))))

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
