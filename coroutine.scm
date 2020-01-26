
;; push 'val' to the end of the queue represented by 'p'
;; (where 'p' is a pair and (car p) is the head of a
;; list and (cdr p) is the end of it)
(: queue-push ((pair (list-of procedure) (list-of procedure)) procedure -> undefined))
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
(define *cont-queue* '(() . ()))

;; continuations parked on wait(2)
(define *wait-tab* (make-hash-table #:test = #:hash number-hash))

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

(: make-semaphore (fixnum -> (vector fixnum pair)))
(define (make-semaphore n)
  (vector n '(() . ())))

(: semacquire ((vector fixnum pair) -> undefined))
(define (semacquire s)
  (let ((v (vector-ref s 0)))
    (if (<= v 0)
      (queue-wait! (vector-ref s 1))
      (vector-set! s 0 (- v 1)))))

(: semrelease ((vector fixnum pair) -> undefined))
(define (semrelease s)
  (let ((q (vector-ref s 1)))
    (if (null? (car q))
      (vector-set! s 0 (+ (vector-ref s 0) 1))
      (pushcont! (queue-pop! q)))))

(: with-semaphore ((vector fixnum pair) (-> 'a) -> 'a))
(define (with-semaphore s thunk)
  (semacquire s)
  (let ((v (thunk)))
    (semrelease s)
    v))

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

;; spawn runs (apply thunk args) asynchronously
;; and returns an opaque object that can be used
;; to query the status of the procedure
(: spawn (procedure #!rest * -> (vector symbol * list)))
(define (spawn proc . args)
  (let ((box (vector 'started #f '())))
    ;; procedure to run when the continuation completes
    (define (procexit status value)
      (vector-set! box 0 status)
      (vector-set! box 1 value)
      (let loop ((lst (vector-ref box 2)))
        (or (null? lst)
            (let ((ret (car lst)))
              (pushcont! (lambda () (ret value)))
              (loop (cdr lst)))))
      (vector-set! box 2 #f)
      (%yield))
    ;; exception handler for dynamic extent of continuation
    (define (on-exception exn)
      (print-error-message exn)
      (procexit 'exn exn))
    (call/cc
      (lambda (ret)
        (pushcont! (lambda () (ret box)))
        (procexit 'done (parameterize ((current-exception-handler on-exception))
                          (apply proc args)))))))

(: %poll (-> undefined))
(define (%poll)
  (when (= 0 (hash-table-size *wait-tab*))
    (error "poll but no procs to wait on?")) ;; implies deadlock
  (let-values (((pid ok status) (process-wait)))
    (let ((target (hash-table-ref/default *wait-tab* pid #f)))
      (if target
        (begin
          (hash-table-delete! *wait-tab* pid)
          (pushcont! (lambda () (target pid ok status))))
        (begin
          (info "warning: pid not registered?" pid)
          (%poll))))))

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

