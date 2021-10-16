(import
 scheme
 matchable
 (distill plan)
 (distill filepath)
 (distill package)
 (distill service)
 (distill unix)
 (chicken string))

(include "test-helpers.scm")

(define my-service
  (make-service
   name: 'my-service
   users: (list (adduser 'foo group: 'foo))
   groups: (list (addgroup 'foo '(foo)))
   spec:   (oneshot*
            up: '(echo "foo is up!"))))

(define (have-line-prefix pre text)
  (let loop ((lines (string-split text "\n")))
    (and (not (null? lines))
         (or (string-prefix? pre (car lines))
             (loop (cdr lines))))))

(define my-group
  (make-group
   name:  'unpriv
   gid:   1000
   users: '(unpriv)))

(define my-user
  (make-user
   name: 'unpriv
   uid:  1000
   gid:  (group-gid my-group)
   login: "/bin/sh"))

(define (has-prefixes str)
  (and (have-line-prefix "foo:x:" str)
       (have-line-prefix "unpriv:x:1000:" str)))

;; test that 'foo is in /etc/fstab
(let ((pkgs (services->packages (list my-service) (list my-user) (list my-group))))
  (define (check-etc-passwd str)
    (or (has-prefixes str)
        (error "etc/passwd doesn't have user foo:" str)))
  (define (check-etc-group str)
    (or (has-prefixes str)
        (error "etc/group doesn't have group foo:" str)))
  (let loop ((pkgs pkgs)
             (seen-passwd #f)
             (seen-group #f))
    (if (null? pkgs)
        (or (and seen-passwd seen-group)
            (error "didn't see /etc/fstab or /etc/group anywhere...?"))
        (let ((p (car pkgs)))
          (match p
                 (`#(#(file "/etc/passwd" ,mode) ,h (inline . ,contents))
                  (begin
                    (check-etc-passwd contents)
                    (loop (cdr pkgs) #t seen-group)))
                 (`#(#(file "/etc/group" ,mode) ,h (inline . ,contents))
                  (begin
                    (check-etc-group contents)
                    (loop (cdr pkgs) seen-passwd #t)))
                 (else
                  (loop (cdr pkgs) seen-passwd seen-group)))))))
