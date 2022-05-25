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
            (run prog "-s" "-S" "-o" dst "-L" url))))
       (else (fatal "neither curl(1) nor wget(1) in $PATH"))))))

;; symlink-from-directory can be supplied
;; to user-fetch-hook to cause objects to be
;; "fetched" from another directory by simply
;; symlinking the target artifacts into the
;; destination
(define (symlink-from-directory dir)
  (lambda (hash)
    (lambda (dst)
      (info "symlink" (filepath-join dir hash) dst)
      (create-symbolic-link (filepath-join dir hash) dst))))

;; user-fetch-hook can be used to
;; pick the URL used to fetch an artifact,
;; or it can fetch an artifact all on its own.
(define user-fetch-hook
  (make-parameter #f))

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
      (begin
        (info "fetching" url)
        ((force %fetch) url dst))))

(: with-locked-hash (string (-> 'a) -> 'a))
(define with-locked-hash
  (let ((lock (make-keyed-lock)))
    (lambda (hash thunk)
      (with-locked-key lock hash thunk))))

;; fetcher returns a function of one argument
;; that loads the given src+hash into the destination
;; filepath and returns either #t if the file was
;; downloaded successfully (and the hash matches)
;; or otherwise #f
;;
;; if no user-fetch-hook has been provided,
;; then the returned procedure will simply use
;;   (and (fetch src dst)
;;        (equal? (hash-file dst) hash))
;;
;; if a user-fetch-hook is set and the src is set,
;; then the returned procedure will first attempt
;;   (fetch src dst)
;; followed by the user-fetch-hook
;;
;; if src is #f and no user-fetch-hook is set,
;; then fetcher will exit unrecoverably
(: fetcher ((or string false) string -> (string -> boolean)))
(define (fetcher src hash)
  (define (hook-fetcher hook src hash)
    (let ((ret (hook hash)))
      (cond
       ;; if (hook hash) returns a url,
       ;; then use that as the fetch url
       ((string? ret)
        (lambda (dst)
          (call/cc
           (lambda (return)
             (parameterize ((current-exception-handler
                             (lambda (exn)
                               (info "couldn't fetch" ret)
                               (return #f))))
               (and (fetch ret dst)
                    (equal? (hash-file dst) hash)))))))
       ;; if (hook hash) returns a prodcedure,
       ;; then it should accept the destination path
       ((procedure? ret)
        (lambda (dst)
          (and (ret dst)
               (equal? (hash-file dst) hash))))
       (else
        (error "unexpected return value from user-fetch-hook" ret)))))
  (let ((hook (user-fetch-hook)))
    (cond
     (src
      (lambda (dst)
        (or (and (fetch src dst)
                 (equal? (hash-file dst) hash))
            (and hook
                 (begin
                   (info "falling back to user-fetch-hook for" src)
                   ((hook-fetcher hook src hash) dst))))))
     (hook (hook-fetcher hook src hash))
     (else (fatal "artifact" hash "has src #f and no user-fetch-hook is set")))))

;; we track hashes that have failed to fetch
;; so that we don't try them more than once
;; (since they will often show up more than
;; once in the dependency graph)
(define *hash-unavailable*
  (make-hash-table test: string=? hash: string-hash))

(: fetch-artifact ((or false string) string string #!optional (or false (-> *)) -> *))
(define (fetch-artifact src dstdir hash #!optional (on-failure #f))
  (let* ((download (fetcher src hash))
         (dst      (filepath-join dstdir hash))
         (tmp      (string-append dst ".tmp"))
         (fail     (or on-failure
                       (lambda ()
                         (fatal "download from" src "failed")))))
    (with-locked-hash
     hash
     (lambda ()
       ;; make sure the destination tmpfile
       ;; doesn't exist partially-written
       ;; from a previous failed fetch
       (delete-file* tmp)
       (or (file-exists? dst)
           (if (hash-table-ref/default *hash-unavailable* hash #f)
               (fail)
               (if (download tmp)
                   (rename-file tmp dst #t)
                   (begin
                     (when src
                       (info "for" src "unexpected content hash" (hash-file tmp)))
                     (delete-file* tmp)
                     (hash-table-set! *hash-unavailable* hash #t)
                     (fail)))))))))
