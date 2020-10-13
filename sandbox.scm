(define current-jobserver
  (make-parameter #f))

;; call a thunk with a fresh jobserver bound to current-jobserver
;; during the dynamic extent of the thunk
(: with-new-jobserver ((-> 'a) -> 'a))
(define (with-new-jobserver thunk)
  (let ((js (fdpipe)))
    (with-cleanup
     (lambda ()
       (fdclose (car js))
       (fdclose (cadr js)))
     (lambda ()
       (parameterize ((current-jobserver js))
         (thunk))))))

;; add one job to the jobserver
(: jobserver+ (fixnum -> *))
(define (jobserver+ n)
  (let* ((pipe (current-jobserver))
         (wfd  (cadr pipe))
         (bv   (make-string n #\a)))
    (fdwrite wfd bv)))

;; subtract one job from the jobserver
(: jobserver- (fixnum -> *))
(define (jobserver- n)
  (or (fx<= n 0)
      (let* ((rfd   (car (current-jobserver)))
             (buf   (make-string n))
             (ret   (fdread rfd buf)))
        (cond
         ((fx= ret 0) (error "pipe: EOF"))
         ((fx< ret 0) (error "errno:" (- ret)))
         (else        (jobserver- (fx- n ret)))))))

;; call a thunk with the jobserver
;; decremented for the duration of the call
;;
;; (the jobserver will remain decremented
;; until this call returns ordinarily or
;; through an exception; this does *not*
;; track dynamic extent)
(: call-with-job ((-> 'a) -> 'a))
(define (call-with-job proc)
  (jobserver- 1)
  (with-cleanup
   (let ((exited #f))
     (lambda ()
       ;; guard against deadlock from bad bookkeeping:
       (if exited
           (error "dynamic extent exited twice through exn")
           (begin (set! exited #t) (jobserver+ 1)))))
   proc))

(: bwrap-program (-> string))
(define bwrap-program
  (let ((pro (delay (pathfind "bwrap"))))
    (lambda ()
      (or (force pro)
          (fatal "bwrap(1) not installed in $PATH")))))

;; perform an elaborate chroot into 'root'
;; and then run '/build' inside that new
;; root, with stdout and stderr redirected
;; to 'logfile' (either a file path or file descriptor)
(: sandbox-run (string string -> undefined))
(define (sandbox-run root logfile)
  (let ((bwrap (bwrap-program)) ;; FIXME
        (js    (current-jobserver))
        (args  (list
                "--unshare-ipc"
                "--unshare-pid"
                "--unshare-uts"
                "--unshare-cgroup-try"
                "--unshare-net"
                "--hostname" "builder"
                "--bind" root "/"       ; rootfs containing host tools
                "--dir" "/dev"
                "--dir" "/proc"
                "--dir" "/tmp"
                "--dev" "/dev"
                "--proc" "/proc"
                "--tmpfs" "/tmp"
                "--"
                "/build"))
        ;; DO NOT CHANGE THIS LIGHTLY:
        ;; it may cause builds to fail
        ;; to reproduce!
        (env   '(("PATH" . "/bin:/sbin:/usr/bin:/usr/sbin:/usr/local/bin")
                 ("LC_ALL" . "C.UTF-8")
                 ("SOURCE_DATE_EPOCH" . "0")
                 ("MAKEFLAGS" . "--jobserver-auth=5,6")))
        (setfd! (lambda (fd fromfd)
                  (or (= fd fromfd)
                      (begin
                        (duplicate-fileno fromfd fd)
                        (file-close fromfd))))))
    (let-values (((pid ok status)
                  (process-wait/yield
                   (process-fork
                    (lambda ()
                      ;; any exceptions in here should immediately exit
                      (current-exception-handler (lambda (exn)
                                                   (print-error-message exn)
                                                   (fatal "(execing bwrap):" exn)))
                      (setfd! 6 (cadr js))
                      (setfd! 5 (car js))
                      (setfd! fileno/stdin (file-open "/dev/null" open/rdonly))
                      ;; can't use fdpipe here because we need a *blocking* pipe;
                      (let-values (((rd wr) (create-pipe)))
                        (process-fork
                         (lambda ()
                           (current-exception-handler
                            (lambda (exn)
                              (print-error-message exn)
                              (fatal "(execing zstd):" exn)))
                           (for-each file-close '(5 6))
                           (file-close wr)
                           (setfd! fileno/stdin rd)
                           (process-execute "zstd" (list "-q" "-" "-o" logfile))))
                        (file-close rd)
                        (setfd! fileno/stdout wr))
                      (duplicate-fileno fileno/stdout fileno/stderr)
                      (process-execute bwrap args env))))))
      (or (and ok (= status 0))
          (error "sandbox build failed")))))
