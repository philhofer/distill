(import
  r7rs)

(include "eprint.sld")
(include "coroutine.sld")

(import
  scheme
  (scheme base)
  (chicken process)
  (chicken condition)
  (srfi 69)
  (distill coroutine)
  (distill eprint))

(include "test-helpers.scm")

(define (bin/true arg)
  (let-values (((pid ok status) (process-wait/yield (process-run "/bin/true" '()))))
    (if ok
      arg
      (error "/bin/true exited with" status))))

;; for testing exceptions
(define (bin/true/abort exn)
  (let-values (((pid ok status) (process-wait/yield (process-run "/bin/true" '()))))
    (if ok
      (proc-abort exn)
      (error "/bin/true exited with" status))))

;; simple case: no yielding at all
(test 100 (join/value (spawn (lambda (arg) arg) 100)))

(let ((s0 (spawn bin/true 1)))
  (test 1 (join/value s0)))

(let* ((s0 (spawn bin/true 1))
       (s1 (spawn bin/true 2))
       (s2 (spawn bin/true 3))
       (s3 (spawn bin/true 4)))
  (test* = join/value ((1 s0)
                       (2 s1)
                       (3 s2)
                       (4 s3))))

(let* ((s0 (spawn bin/true 1))
       (s1 (spawn join/value s0))
       (s2 (spawn join/value s1))
       (s3 (spawn join/value s2)))
  (test* = join/value ((1 s0)
                       (1 s1)
                       (1 s2)
                       (1 s3))))

(let* ((sem (make-semaphore 3))
       (lok (make-semaphore 1))
       (run (lambda (arg)
              (semacquire sem)
              (semacquire lok)
              (semrelease lok)
              (semrelease sem)
              arg))
       (_   (semacquire lok))
       (s0  (spawn run 0))
       (s1  (spawn run 1))
       (s2  (spawn run 2))
       (ex? (lambda (p) (not (eq? 'done (proc-status p)))))
       ;; all of these tasks should be blocked
       (_   (test* eq? ex? ((#t s0)
                            (#t s1)
                            (#t s2))))
       (_   (semrelease lok)))
  (test* = join/value ((0 s0)
                       (1 s1)
                       (2 s2))))

;; wrap wait(2) scheduling with a semaphore
(let* ((sem (make-semaphore 2))
       (run (lambda (arg)
              (semacquire sem)
              (let ((res (bin/true arg)))
                (semrelease sem)
                arg)))
       (s0  (spawn run 0))
       (s1  (spawn run 1))
       (s2  (spawn run 2))
       (s3  (spawn run 3))
       (s4  (spawn bin/true/abort "irritant"))
       (s5  (spawn bin/true/abort "another irritant")))
  (test* = join/value ((0 s0)
                       (1 s1)
                       (2 s2)
                       (3 s3)))
  (test equal? "irritant" (join/value s4))
  (test 'exn (proc-status s4))
  (test equal? "another irritant" (join/value s5))
  (test 'exn (proc-status s5)))

(let* ((sem (make-semaphore 5))
       (s0  (spawn (lambda ()
                     (let ((n (semacquire/max sem 7)))
                       (test = n 5)
                       (semrelease/n sem 5)))))
       (s1  (spawn (lambda ()
                     (let ((n (semacquire/max sem 3)))
                       (test = n 3)
                       (semrelease/n sem 3))))))
  (join/value s0)
  (join/value s1)
  (test = 5 (semacquire/max sem 100)))

(display "test OK.\n")
