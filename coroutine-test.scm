(include "test-helpers.scm")

(import
 scheme
 (srfi 12)
 (srfi 69)
 (chicken process)
 (distill eprint)
 (distill coroutine))

(define (bin/true arg)
  (let-values (((pid ok status) (process-wait/yield (process-run "/bin/true" '()))))
    (if ok
        arg
        (error "/bin/true exited with" status))))

;; for testing exceptions
(define (bin/true/abort exn)
  (let-values (((pid ok status) (process-wait/yield (process-run "/bin/true" '()))))
    (if ok
        (abort exn)
        (error "/bin/true exited with" status))))

(define (read-exact fd n)
  (let ((buf (make-string n)))
    (let loop ((rd 0))
      (or (= rd n)
          (let ((ret (fdread fd buf (- n rd))))
            (if (= ret 0)
                (error "unexpected EOF")
                (loop (+ rd ret))))))))

(define (write-full fd str)
  (let ((ret (fdwrite fd str)))
    (or (= ret (string-length str))
        (let ((sub (##sys#substring str ret (string-length str))))
          (write-full fd sub)))))

;; simple case: no yielding at all
(test 100 (join/value (spawn (lambda (arg) arg) 100)))

(let ((s0 (spawn bin/true 1)))
  (test 1 (join/value s0)))

(let* ((seen #f)
       (box  (with-spawn bin/true (list 1)
                         (lambda (box)
                           (set! seen box)))))
  (test eq? seen box)
  (test 1 (join/value box)))

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

(let ((s4  (spawn bin/true/abort "irritant"))
      (s5  (spawn bin/true/abort "another irritant")))
  (test equal? "irritant" (join/value s4))
  (test 'exn (proc-status s4))
  (test equal? "another irritant" (join/value s5))
  (test 'exn (proc-status s5)))

;; shove a bunch of data through a pipe
;; and make sure all of it gets through;
;; in order to exercise fdwrite+EAGAIN,
;; we need to write more than PIPE_BUF bytes (typically 64kB)
(let* ((pipefd (fdpipe))
       (rd     (car pipefd))
       (wr     (cadr pipefd))
       (ilen   289)
       (times  1000)
       (reader (spawn (lambda ()
                        (let loop ((i 0))
                          (or (>= i times)
                              (begin (read-exact rd ilen) (loop (+ 1 i))))))))
       (writer (spawn (lambda ()
                        (let loop ((i 0))
                          (or (>= i times)
                              (begin (write-full wr (make-string ilen #\a)) (loop (+ 1 i)))))))))
  (test eq? #t (join/value writer))
  (test eq? #t (join/value reader)))

(test string=? "foobar" (call/cc
                         (lambda (ret)
                           (parameterize ((current-exception-handler ret))
                             (push-exception-wrapper
                              (lambda (exn)
                                (string-append "foo" exn))
                              (lambda ()
                                (abort "bar")
                                #f))))))

(let* ((lock   (make-keyed-lock))
       (key    "the-key")
       (value  #f)
       (inside (lambda ()
                 (with-locked-key
                  lock key
                  (lambda ()
                    (when value
                      (error "inside should equal #f"))
                    (set! value #t)
                    (bin/true #t)
                    (set! value #f)
                    #t))))
       (s0     (spawn inside))
       (s1     (spawn inside)))
  (test eq? #t (join/value s0))
  (test eq? #t (join/value s1)))

(display "test OK.\n")
