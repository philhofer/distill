(define-kvector-type
  <user>
  make-user
  user?
  (user-name  name:  #f symbol?)
  (user-uid   uid:   #f fixnum?)
  (user-gid   gid:   #f fixnum?)
  (user-home  home:  "/var/empty" string?)
  (user-login login: "/sbin/nologin" string?))

(define-kvector-type
  <group>
  make-group
  group?
  (group-name  name:  #f  symbol?)
  (group-gid   gid:   #f  fixnum?)
  (group-users users: '() (list-of symbol?)))

(: base-users (list-of vector))
(define base-users
  (map
   (cut apply make-user <>)
   '((name: root     uid: 0     gid: 0     home: "/root" login: "/bin/ash")
     (name: nobody   uid: 65534 gid: 65534 home: "/"     login: "/sbin/nologin")
     (name: catchlog uid: 99    gid: 99    home: "/"     login: "/sbin/nologin"))))

(: base-groups (list-of vector))
(define base-groups
  (map
   (cut apply make-group <>)
   '((name: root     gid: 0     users: (root))
     (name: nobody   gid: 65534 users: (nobody))
     (name: catchlog gid: 99    users: (catchlog)))))

(define (adduser name #!key
                 (group #f)
                 (home "/var/empty")
                 (login "/sbin/nologin"))
  (lambda (uid getgroup)
    (make-user
     name:  name
     uid:   uid
     gid:   (if group (getgroup group) uid)
     home:  home
     login: login)))

(define (addgroup name users)
  (lambda (gid)
    (make-group
     name: name
     gid:  gid
     users: users)))

;; groups+users->artifacts takes a list
;; of addgroup expressions and a list of
;; adduser expressions and returns a list
;; of artifacts for /etc/passwd, /etc/group, and so forth
;;
;; the groups and users lists can either be
;; the result of the 'adduser' or 'addgroup'
;; functions, respectively, or they can be
;; the result of make-user and make-group,
;; respectively
(define (groups+users->artifacts gps ups #!key
                                 (start-uid 100)
                                 (start-gid 100))
  (let* ((ht       (make-hash-table test: eq? hash: eq?-hash))
         (getgroup (lambda (name)
                     (if (integer? name)
                         name
                         (hash-table-ref ht name))))
         (groups   (let loop ((in  gps)
                              (out '())
                              (gid start-gid))
                     (if (null? in)
                         out
                         (let ((head (car in)))
                           (if (group? head)
                               (begin
                                 (hash-table-set! ht (group-name head) (group-gid head))
                                 (loop (cdr in) (cons head out) gid))
                               (let ((g ((car in) gid)))
                                 (hash-table-set! ht (group-name g) gid)
                                 (loop (cdr in) (cons g out) (+ 1 gid))))))))
         (users    (let loop ((in  ups)
                              (out '())
                              (uid start-uid))
                     (if (null? in)
                         out
                         (let ((head (car in)))
                           (cond
                            ((user? head)
                             (loop (cdr in) (cons head out) uid))
                            ((procedure? head)
                             (loop (cdr in) (cons (head uid getgroup) out) (+ 1 uid)))
                            (else (error "bad user spec" head))))))))
    (list
     (etc/passwd (append base-users users))
     (etc/group  (append base-groups groups)))))

;; etc/passwd produces the artifact for /etc/passwd
;; given a list of users
(: etc/passwd ((list-of vector) --> vector))
(define (etc/passwd users)
  (let ((usr->line (lambda (u)
                     (list
                      (user-name u)
                      "x"
                      (user-uid u)
                      (user-gid u)
                      ""
                      (user-home u)
                      (user-login u)))))
    (interned "/etc/passwd"
              #o644
              (tabular usr->line ":" "\n" users))))

(: etc/group ((list-of vector) --> vector))
(define (etc/group groups)
  (let ((grp->line (lambda (g)
                     (list
                      (group-name g)
                      "x"
                      (number->string (group-gid g))
                      (join-with "," (group-users g))))))
    (interned "/etc/group"
              #o644
              (tabular grp->line ":" "\n" groups))))
