(import
 scheme
 (only (chicken base) error)
 (only (chicken port) with-output-to-string)
 (only (chicken module) export)
 (distill plan)
 (distill kvector)
 (distill execline)
 (distill service)
 (distill text))

;; getty produces a service that
;; runs "getty <args ...>"
(define (getty . args)
  (make-service
   ;; produce a name like getty-ttyS0, getty-tty0, etc.
   name: (let ((tty (let loop ((args args)) ; extract last list element
                      (cond
                       ((null? args)
                        (error "getty missing tty argument"))
                       ((null? (cdr args))
                        (car args))
                       (else
                        (loop (cdr args)))))))
           (string->symbol
            (join-with "-" (list 'getty tty))))
   spec: (longrun*
          down-signal: 1
          run: (cons 'getty args))))

(define console-login
  (interned "/sbin/console-login" #o744
            (with-output-to-string
              (lambda ()
                (write-exexpr '(cd /root /bin/sh -l))))))

;; console-root-shell launches a getty
;; that listens on a particular tty
;; with no login password
;;
;; NOTE: this is a terrible thing to do
;; on a serial port with the wrong permissions
;; or one that is otherwise exposed to the world!
(export console-root-shell)
(define (console-root-shell #!key (speed 115200) (tty 'ttyS0))
  (make-service
   name: (string->symbol
          (string-append
           "getty-console-" (symbol->string tty)))
   inputs: (list console-login)
   spec: (longrun*
          ;; this is going to start a shell process,
          ;; which will happily ignore SIGTERM,
          ;; so use SIGHUP here instead
          down-signal: 1
          run: `(getty -n -l /sbin/console-login ,speed ,tty))))
