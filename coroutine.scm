(foreign-declare #<<EOF
#include <stdbool.h>
#include <stdint.h>
#include <signal.h>
#include <unistd.h>
#include <fcntl.h>
#include <poll.h>
#include <sys/eventfd.h>
#include <err.h>
static int chldfd;
static void handle_sigchld(int sig)
{
    int64_t val;
    int rc;

    val = 1;
    do {
        rc = write(chldfd, &val, sizeof(val));
    } while (rc < 0 && errno == EINTR);
    /* terribly bad ... */
    if (rc != sizeof(val)) err(1, "write(eventfd)");
}
static int sigchld_handler(void)
{
    struct sigaction act;
    if ((chldfd = eventfd(0, EFD_CLOEXEC|EFD_NONBLOCK)) < 0)
        return -errno;

    act = (struct sigaction){
        .sa_handler = handle_sigchld,
        .sa_flags = SA_NOCLDSTOP,
    };
    if (sigaction(SIGCHLD, &act, NULL) < 0) return -errno;
    return chldfd;
}
static int do_poll(int32_t *rfd, int nrfd, int32_t *wfd, int nwfd, bool block)
{
    struct pollfd *pfd;
    int ret, i;

    pfd = calloc(nrfd+nwfd, sizeof(struct pollfd));
    if (!pfd) err(1, "out of memory");

    for (i=0; i<nrfd; i++) {
        pfd[i].fd = rfd[i];
        pfd[i].events = POLLIN|POLLERR|POLLHUP;
    }
    for (i=nrfd; i<nrfd+nwfd; i++) {
        pfd[i].fd = wfd[i-nrfd];
        pfd[i].events = POLLOUT|POLLERR|POLLHUP;
    }
again:
    ret = poll(pfd, nrfd+nwfd, block ? -1 : 0);
    if (!ret) goto done;
    if (ret < 0) {
        if ((errno == EAGAIN || errno == EINTR) && block) goto again;
        ret = block ? -errno : 0;
        goto done;
    }
    for (i=0; i<nrfd; i++) {
        if (!pfd[i].revents) rfd[i]=-1;
    }
    for (i=nrfd; i<nrfd+nwfd; i++) {
        if (!pfd[i].revents) wfd[i]=-1;
    }
done:
    free(pfd);
    return ret;
}
EOF
)

;; push 'val' to the end of the queue represented by 'p'
;; (where 'p' is a pair and (car p) is the head of a
;; list and (cdr p) is the end of it)
(: queue-push! ((pair (list-of procedure) (list-of procedure)) procedure -> undefined))
(define (queue-push! p val)
  (let ((tail (cdr p))
        (end  (cons val '())))
    (if (null? tail)
      (set-car! p end)
      (set-cdr! tail end))
    (set-cdr! p end)))

(: queue-pop ((pair (list-of procedure) (list-of procedure)) -> (or procedure false)))
(define (queue-pop! p)
  (let ((head (car p)))
    (and (pair? head)
         (let ((first (car head))
               (rest  (cdr head)))
           (set-car! p rest)
           (when (null? rest)
             (set-cdr! p '()))
           first))))

;; runnable continuations
(: *cont-queue* (pair (list-of procedure) (list-of procedure)))
(define *cont-queue* (cons '() '()))

;; continuations parked on wait(2)
(define *wait-tab* (make-hash-table test: = hash: number-hash))
(define *readfd-tab* (make-hash-table test: = hash: number-hash))
(define *writefd-tab* (make-hash-table test: = hash: number-hash))

(define-constant eintr 4)
(define-constant eagain 11)

(: fdclose (fixnum -> fixnum))
(define (fdclose fd)
  ((foreign-lambda int close int) fd))

(: fdpipe (-> (list fixnum fixnum)))
(define (fdpipe)
  (let* ((vect (make-s32vector 2 -1))
         (ret  ((foreign-lambda* int ((s32vector pfd))
                  "C_return(pipe2(pfd, O_NONBLOCK));")
                vect)))
    (if (fx= ret 0)
      (list (s32vector-ref vect 0) (s32vector-ref vect 1))
      (error "pipe2() failed"))))

(: fdwrite (fixnum (or string u8vector) #!rest * -> integer))
(define (fdwrite fd buf #!optional size)
  (let* ((%raw-write (foreign-lambda* long ((int fd) (scheme-pointer mem) (size_t sz))
                       "long out; out=write(fd,mem,sz); C_return(out>0?out:-errno);"))
         (buflen (cond
                   ((string? buf) (string-length buf))
                   ((u8vector? buf) (u8vector-length buf))
                   (else (error "bad buf argument to fdwrite:" buf))))
         (size   (or size buflen)))
    (let loop ((ret (%raw-write fd buf size)))
      (cond
        ((>= ret 0) ret)
        ((= ret (- eintr))
         (loop (%raw-write fd buf size)))
        ((= ret (- eagain))
         (begin
           (queue-wait!
             (hash-table-update!/default
               *writefd-tab*
               fd
               identity
               (cons '() '())))
           (loop (%raw-write fd buf size))))
        (else (error "fdwrite: errno:" (- ret)))))))

(: fdread (fixnum (or string u8vector) #!rest * -> integer))
(define (fdread fd buf #!optional size)
  (let* ((%raw-read  (foreign-lambda* long ((int fd) (scheme-pointer mem) (size_t sz))
                       "long out; out=read(fd,mem,sz); C_return(out>=0?out:-errno);"))
         (buflen (cond
                   ((string? buf) (string-length buf))
                   ((u8vector? buf) (u8vector-length buf))
                   (else (error "bad buf argument to fdread:" buf))))
         (size   (or size buflen)))
    (let loop ((ret (%raw-read fd buf size)))
      (cond
        ((>= ret 0) ret)
        ((= ret (- eintr))
         (loop (%raw-read fd buf size)))
        ((= ret (- eagain))
         (begin
           (queue-wait!
             (hash-table-update!/default
               *readfd-tab*
               fd
               identity
               (cons '() '())))
           (loop (%raw-read fd buf size))))
        (else (error "fdread: errno:" (- ret)))))))

(define (%poll-fds)
  (let ((%raw-poll (foreign-lambda int do_poll s32vector int s32vector int bool))
        (rfds      (list->s32vector (hash-table-keys *readfd-tab*)))
        (wfds      (list->s32vector (hash-table-keys *writefd-tab*)))
        (flush     (lambda (vec ht)
                     (let ((len (s32vector-length vec)))
                       (let loop ((i 0))
                         (or (fx>= i len)
                             (let ((fd (s32vector-ref vec i)))
                               (if (fx= fd -1)
                                 (loop (fx+ i 1))
                                 (let ((q  (hash-table-ref ht fd)))
                                   (hash-table-delete! ht fd)
                                   (let inner ((cont (queue-pop! q)))
                                     (and cont (begin (pushcont! cont) (inner (queue-pop! q)))))
                                   (loop (fx+ i 1)))))))))))
    (when (= 0 (+ (s32vector-length rfds) (s32vector-length wfds)))
      (fatal "deadlock"))
    ;; first, do a non-blocking poll; if nothing is immediately ready,
    ;; then perform a major GC and try again
    (let again ((block #f))
      ;; do_poll() doesn't modify the fd vectors if ret==0,
      ;; so we can safely re-use them when no fds are ready
      (let ((ret (%raw-poll rfds (s32vector-length rfds)
                            wfds (s32vector-length wfds)
                            block)))
        (cond
          ((fx= 0 ret) (begin (gc #f) (again #t)))
          ((fx> 0 ret) (error "poll error:" (- ret)))
          (else
            (begin
              (flush rfds *readfd-tab*)
              (flush wfds *writefd-tab*))))))))

;; push a continuation onto the tail of the cont-queue
(: pushcont! (procedure -> undefined))
(define (pushcont! p)
  (queue-push! *cont-queue* p))

;; pop a continuation off of the front of the cont-queue
(: popcont! (-> (or false procedure)))
(define (popcont!)
  (queue-pop! *cont-queue*))

;; %yield is a local continuation exit; it does not return
(define (%yield)
  (let ((cont (popcont!)))
    (if cont
      (begin (cont) (error "longjmp returned?"))
      (begin (%poll) (%yield)))))

(: queue-wait! (pair -> undefined))
(define (queue-wait! p)
  (call/cc
    (lambda (ret)
      (queue-push! p (lambda () (ret #t)))
      (%yield))))

;; process-wait/yield is the semantically the same
;; as chicken.process#process-wait, except that it
;; suspends the current coroutine while waiting
(define (process-wait/yield pid)
  (call/cc
   (lambda (resume)
     (hash-table-set! *wait-tab* pid resume)
     (%yield))))

;; proc-status is one of:
;;  'started (running or paused in a continuation)
;;  'exn     (terminated with an exception)
;;  'done    (terminated with a value)
(: proc-status ((vector symbol * list) -> symbol))
(define (proc-status box)
  (vector-ref box 0))

;; see (spawn ...)
(: proc-return ((vector symbol 'a list) -> 'a))
(define (proc-return box)
  (vector-ref box 1))

(define (%procexit box status value)
  (vector-set! box 0 status)
  (vector-set! box 1 value)
  (for-each
    (lambda (ret)
      (pushcont! (lambda () (ret value))))
    (vector-ref box 2))
  (vector-set! box 2 #f)
  (%yield))

;; spawn runs (apply thunk args) asynchronously
;; and returns an opaque object that can be used
;; to query the status of the procedure
(: spawn (procedure #!rest * -> (vector symbol * list)))
(define (spawn proc . args)
  (let ((box (vector 'started #f '())))
    (call/cc
      (lambda (ret)
        (pushcont! (lambda () (ret box)))
        (%procexit box 'done
                   (parameterize ((current-exception-handler
                                    (lambda (exn)
                                      (%procexit box 'exn exn))))
                     (apply proc args)))))))

(define (wait-any-nohang)
  (call/cc
   (lambda (ret)
     (parameterize ((current-exception-handler (lambda (exn) (ret 0 #f #f))))
       (process-wait -1 #t)))))

;; %pid-poll reads from the chldfd eventfd
;; and then calls wait() repeatedly until there are no
;; child processes outstanding
(define (%pid-poll fd)
  (let ((getcount (foreign-lambda* int ((int fd) (s64vector buf))
		    "C_return(read(fd,(int64_t *)buf,8)==8 ? 0 : errno);"))
	(buf      (make-s64vector 1 0)))
    (let loop ((err (getcount fd buf)))
      (cond
       ((= err 0) ;; happy case (note that we're ignoring the counter ...)
	(let-values (((pid ok status) (wait-any-nohang)))
	  (cond
	   ((= pid 0) (loop (getcount fd buf)))
	   ((hash-table-ref/default *wait-tab* pid #f)
	    => (lambda (cont)
		 (hash-table-delete! *wait-tab* pid)
		 (pushcont! (lambda () (cont pid ok status)))
		 (loop 0)))
	   (else
	    (info "warning: pid not registered?" pid)
	    (loop 0)))))
       ((= err eintr)
	(loop (getcount fd buf)))
       ((= err eagain)
	(begin
	  (queue-wait!
	   (hash-table-update!/default
	    *readfd-tab*
	    fd
	    identity
	    (cons '() '())))
	  (loop (getcount fd buf))))
       (else
	(error "fatal errno from read(eventfd)" fd err (s64vector-ref buf 0)))))))

(: %poll (-> undefined))
(define (%poll)
  (if (eq? (proc-status pid-poller-proc) 'exn)
    (begin
      (print-error-message (proc-return pid-poller-proc))
      (fatal "pid poller exited!"))
    (%poll-fds)))

;; join/value waits for a coroutine to exit,
;; then yields its return value
;;
;; note that if the coroutine threw an exception,
;; the return value will satisfy 'condition?'
(: join/value ((vector fixnum * list) -> *))
(define (join/value proc)
  (if (eq? (proc-status proc) 'started)
    (call/cc
      (lambda (ret)
        (vector-set! proc 2 (cons ret (vector-ref proc 2)))
        (%yield)))
    (proc-return proc)))

;; XXX maybe we should be doing this lazily?
(define pid-poller-proc
  (let* ((handle (foreign-lambda int sigchld_handler))
         (err    (handle)))
    (if (<= err 0)
      (error "error registering SIGCHLD handler:" (- err))
      (spawn %pid-poll err))))

;; push-exception-wrapper installs ((current-exception-handler) (wrap exn))
;; as the current exception handler for the dynamic extent
;; of (thunk)
(define (push-exception-wrapper wrap thunk)
  (let* ((old (current-exception-handler))
         (new (lambda (exn)
                (parameterize ((current-exception-handler old))
                  (old (wrap exn))))))
    (parameterize ((current-exception-handler new))
      (thunk))))

(define (with-cleanup done thunk)
  (push-exception-wrapper
    (lambda (exn) (done) exn)
    (lambda ()
      (let ((res (thunk)))
        (done)
        res))))
