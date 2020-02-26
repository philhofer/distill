(define <user>
  (make-kvector-type
    name:
    uid:
    gid:
    home:
    login:))

(define valid-user?
  (kvector/c
    <user>
    name:  symbol?
    uid:   fixnum?
    gid:   fixnum?
    home:  string?
    login: string?))

(define make-user
  (let ((make (kvector-constructor <user>)))
    (lambda args
      (conform
        valid-user?
        (apply make args)))))

(: user-name (vector --> symbol))
(define user-name  (kvector-getter <user> name:))
(: user-uid (vector --> fixnum))
(define user-uid   (kvector-getter <user> uid:))
(: user-gid (vector -> fixnum))
(define user-gid   (kvector-getter <user> gid:))
(: user-homem (vector -> string))
(define user-home  (kvector-getter <user> home:))
(: user-login (vector -> string))
(define user-login (kvector-getter <user> login:))

(define <group>
  (make-kvector-type
    name:
    gid:
    users:))

(define valid-group?
  (kvector/c
    <group>
    name:  symbol?
    gid:   fixnum?
    users: (list-of symbol?)))

(define make-group
  (let ((make (kvector-constructor <group>)))
    (lambda args
      (conform
        valid-group?
        (apply make args)))))

(: group-name (vector --> symbol))
(define group-name  (kvector-getter <group> name:))
(: group-gid (vector --> fixnum))
(define group-gid   (kvector-getter <group> gid:))
(: group-users (vector --> (list-of symbol)))
(define group-users (kvector-getter <group> users:))

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

(define grp/root
  (make-group
    name: 'root
    gid: 0
    users: '(root)))

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
(define (groups+users->artifacts gps ups #!key
                              (start-uid 100)
                              (start-gid 100))
  (let* ((ht       (make-hash-table test: eq? hash: eq?-hash))
         (getgroup (lambda (name)
                     (hash-table-ref ht name)))
         (groups   (let loop ((in  gps)
                              (out '())
                              (gid start-gid))
                     (if (null? in)
                       out
                       (loop (cdr in)
                             (let ((g ((car in) gid)))
                               (hash-table-set! ht (group-name g) gid)
                               (cons g out))
                             (+ 1 gid)))))
         (users    (let loop ((in  ups)
                              (out '())
                              (uid start-uid))
                     (if (null? out)
                       out
                       (loop (cdr in)
                             (cons ((car in) uid getgroup) out)
                             (+ 1 uid))))))
    (list
      (etc/passwd (append base-users users))
      (etc/group  (append base-groups groups)))))

;; etc/passwd produces the artifact for /etc/passwd
;; given a list of users
(: etc/passwd ((list-of vector) --> vector))
(define (etc/passwd users)
  (let ((usr->line (lambda (u)
                     (string-append
                       (symbol->string (user-name u))
                       ":x:"
                       (number->string (user-uid u))
                       ":"
                       (number->string (user-gid u))
                       "::"
                       (user-home u)
                       ":"
                       (user-login u)))))
    (interned "/etc/passwd"
              #o644
              (lines/s (s/bind (list->seq users) (k/map usr->line))))))

(: etc/group ((list-of vector) --> vector))
(define (etc/group groups)
  (let ((grp->line (lambda (g)
                     (string-append
                       (symbol->string (group-name g))
                       ":x:"
                       (number->string (group-gid g))
                       ":"
                       (join/s "," (list->seq (group-users g)))))))
    (interned "/etc/group"
              #o644
              (lines/s (s/bind (list->seq groups)
                               (k/map grp->line))))))

