(foreign-declare "#include \"archive.h\"")

;; dir->tar.zst creates a deterministic
;; ustar archive of a directory and writes
;; it into dst, returning the base64 blake2b hash
;; of the complete archive
(: dir->tar.zst (string string -> (or string false)))
(define (dir->tar.zst dir dst)
  (let* ((str          (make-string 44))
         (%archive-dir (foreign-lambda int "make_tar_zst" nonnull-c-string nonnull-c-string scheme-pointer))
         (rc           (%archive-dir dir dst str)))
    (and (= rc 0) str)))

;; fork+dir->tar.zst performs the same action
;; as dir->tar.zst, except that it forks before
;; creating the child directory and uses coroutines
;; to reschedule the current continuation while the
;; child process is running
(: fork+dir->tar.zst (string string -> (or string false)))
(define (fork+dir->tar.zst dir name)
  (let-values (((rfd wfd) (create-pipe)))
    (let* ((fds   (fdpipe))
           (rfd   (car fds))
           (wfd   (cadr fds))
           (child (process-fork
                   (lambda ()
                     (fdclose rfd)
                     (duplicate-fileno wfd 1)
                     (fdclose wfd)
                     (display (or (dir->tar.zst dir name)
                                  (fatal "failed creating" name))))))
           (exitok (lambda (pid)
                     (let-values (((pid ok? status) (process-wait/yield pid)))
                       (and ok? (= status 0)))))
           (end    (spawn exitok child)))
      (fdclose wfd)
      (let* ((ret (fdread-exact rfd 44))
             (ok? (join/value end))
             (_   (fdclose rfd)))
        (and ok? (= (string-length ret) 44) ret)))))
