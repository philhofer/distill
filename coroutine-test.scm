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
  (coroutine)
  (eprint))

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
      (error "got exn:" exn)
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
       (s4  (spawn bin/true/abort "irritant")))
  (test* = join/value ((0 s0)
                       (1 s1)
                       (2 s2)
                       (3 s3)))
  (test #t (condition? (join/value s4))))

#;(let* ((q  (new-queue 2))
       (s0 (queue-run q bin/true 1))
       (s2 (queue-run q bin/true/abort "string irritant"))
       (s3 (queue-run q bin/true 2)))
  (test* = proc-wait/value ((1 s0)
                            (2 s3)))
  (test #t (condition? (proc-wait/value s2)))
  (test 0 (queue-running q)))

#;(let* ((q0  (new-queue 2))
       (q1  (new-queue 2))
       (p00 (queue-run q0 bin/true 0))
       (p10 (queue-run q1 bin/true 0))
       (p01 (queue-run q0 bin/true 1))
       (p11 (queue-run q1 bin/true 1)))
  (test* = proc-wait/value ((1 p11)
                            (1 p01)
                            (0 p10)
                            (0 p00)))
  (test* = queue-running ((0 q1)
                          (0 q0)))
  (queue-wait q0)
  (queue-wait q1))

(display "test OK.\n")
