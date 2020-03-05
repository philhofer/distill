(define <longrun>
  (make-kvector-type
    type:
    run:
    finish:
    dependencies:
    notification-fd:
    producer-for:
    consumer-for:))

(define symbolic? (or/c string? symbol?))

(define longrun*
  (kvector-constructor
    <longrun>
    type:   'longrun (eq?/c 'longrun)
    run:    #f       list?
    finish: #f       (or/c false/c list?)
    dependencies:    #f (or/c false/c string?)
    notification-fd: #f (or/c false/c integer?)
    producer-for:    #f (or/c false/c symbolic?)
    consumer-for:    #f (or/c false/c symbolic?)))

(define longrun? (kvector-predicate <longrun>))

(define <oneshot>
  (make-kvector-type
    type:
    up:
    down:
    dependencies:))

(define oneshot*
  (kvector-constructor
    <oneshot>
    type: 'oneshot (eq?/c 'oneshot)
    up:   #f       list?
    down: #f       (or/c false/c list?)
    dependencies: #f (or/c false/c string?)))

(define oneshot? (kvector-predicate <oneshot>))

(define <bundle>
  (make-kvector-type
    type:
    contents:))

(define %make-bundle
  (kvector-constructor
    <bundle>
    type: 'bundle (eq?/c 'bundle)
    contents: #f string?))

(define bundle? (kvector-predicate <bundle>))

(define (bundle* . args)
  (%make-bundle
    contents: (lines/s (s/bind (list->seq args) service-name))))

(define spec?
  (or/c longrun? oneshot? bundle?))

;; spec->artifact-seq takes an s6-rc service specification
;; and yields a sequence of artifacts that need to be present
;; for s6-rc-compile to produce the correct s6-rc database entry
;;
;; e.g. (spec->artifact-seq (s6-svc "foo" up: ... down: ...))
;; produces artifacts for /src/services/foo/up, etc.
(define (spec->artifact-seq name spec)
  (let* ((dir         name)
         (field->file (lambda (kw val)
                        (let ((file (keyword->string kw)))
                          (interned
                            (filepath-join "/src/services" dir file)
                            #o644
                            (with-output-to-string
                              (lambda () (if (pair? val)
                                           (write-exexpr val)
                                           (begin (display val) (newline))))))))))
    (lambda (kons seed)
      (kvector-foldl spec
                     (lambda (kw val lst)
                       (if val
                         (kons (field->file kw val) lst)
                         lst))
                     seed))))

(define s6-rc-db
  (lambda (art-seq)
    (let ((artifacts (art-seq (k/recur cons) '())))
      (lambda (conf)
        (make-package
          label:  "s6-rc-db"
          src:    artifacts
          tools:  (list busybox-core execline-tools s6 s6-rc)
          inputs: '()
          build:  (make-recipe
                    script: (execline*
                              (if ((mkdir -p "/out/etc/s6-rc")))
                              (s6-rc-compile "/out/etc/s6-rc/compiled" "/src/services"))))))))

(define s6
  (let* ((version '2.9.0.1)
         (src     (remote-archive
                    (conc "https://skarnet.org/software/s6/s6-" version ".tar.gz")
                    "uwnwdcxfc7i3LTjeNPcnouhShXzpMPIG0I2AbQfjL_I=")))
    (lambda (conf)
      (make-package
        label:  (conc "s6-" ($arch conf))
        src:    src
        tools:  (cc-for-target conf)
        inputs: (list skalibs execline-tools musl libssp-nonshared)
        build:  (ska-recipe
                  (conc "s6-" version)
                  (kwith
                    ($ska-build conf)
                    configure-args: (+= `(,(conc '--with-sysdeps= ($sysroot conf) "/lib/skalibs/sysdeps")
                                           --enable-static-libc))))))))
(define s6-rc
  (let* ((version '0.5.1.1)
         (src     (remote-archive
                    (conc "https://skarnet.org/software/s6-rc/s6-rc-" version ".tar.gz")
                    "KCvqdFSKEUNPkHQuCBfs86zE9JvLrNHU2ZhEzLYY5RY=")))
    (lambda (conf)
      (make-package
        label:  (conc "s6-rc-" ($arch conf))
        src:    src
        tools:  (cc-for-target conf)
        inputs: (list s6 skalibs execline-tools musl libssp-nonshared)
        build:  (ska-recipe
                  (conc "s6-rc-" version)
                  (kwith
                    ($ska-build conf)
                    configure-args: (+= `(,(conc '--with-sysdeps= ($sysroot conf) "/lib/skalibs/sysdeps")
                                           --enable-static-libc))))))))

(define busybox-full
  (busybox/config
    "KCPbK4zCat3hyqDNYlVR_pKYSAAW0vuDh84mfXqtT6w="
    (list linux-headers)))

(define <service>
  (make-kvector-type
    name:
    inputs:
    users:
    groups:
    after:
    spec:))

(define service? (kvector-predicate <service>))

(: service-inputs (vector --> (list-of (or procedure vector))))
(define service-inputs (kvector-getter <service> inputs:))
(: service-name (vector --> symbol))
(define service-name (kvector-getter <service> name:))
(: service-users (vector --> list))
(define service-users (kvector-getter <service> users:))
(: service-groups (vector --> list))
(define service-groups (kvector-getter <service> groups:))
(: service-after (vector --> (list-of procedure)))
(define service-after (kvector-getter <service> after:))
(: service-spec (vector --> vector))
(define service-spec (kvector-getter <service> spec:))

(: make-service (#!rest * -> vector))
(define make-service
  (kvector-constructor
    <service>
    name:   #f  symbol?
    inputs: '() (list-of (or/c procedure? artifact?))
    users:  '() (list-of procedure?)
    groups: '() (list-of procedure?)
    after:  '() (list-of (or/c procedure? string? symbol?))
    spec:   #f  spec?))

(: update-service (vector #!rest * -> vector))
(define (update-service svc . args)
  (apply make-service
         (append (kvector->list svc) args)))

(: s/packages ((list-of vector) -> procedure))
(define (s/packages lst)
  (s/bind (list->seq lst)
          (kompose
            (k/map service-inputs)
            (k/map list->seq)
            k/recur
            (k/uniq test: eq? hash: eq?-hash))))

(: named* (string -> (vector -> boolean)))
(define (named* pat)
  (let ((sre (glob->sre pat)))
    (lambda (svc)
      (irregex-match? sre (symbol->string (service-name svc))))))

;; service-depends takes a service and
;; a sequence of all service and yields
;; a sequence of services that must be
;; started before 'svc'
(define (service-depends svc all-seq)
  (let ((after (service-after svc)))
    (if (null? after)
      '()
      (let* ((match? (lambda (svc)
                       (let loop ((lst after))
                         (if (null? lst)
                           #f
                           (let ((head (car lst))
                                 (rest (cdr lst)))
                             (or (cond
                                   ((string? head)
                                    (string=? head (symbol->string (service-name svc))))
                                   ((symbol? head)
                                    (eq? head (service-name svc)))
                                   ((procedure? head)
                                    (head svc))
                                   (else (fatal "invalid 'service#after' element" head)))
                                 (loop rest)))))))
             (seq    (s/bind all-seq (k/filter match?))))
        (seq cons '())))))

(define (service->artifact-seq svc svc-seq)
  (let ((depends (service-depends svc svc-seq))
        (spec    (service-spec svc)))
    (spec->artifact-seq
      (service-name svc)
      (if (null? depends)
        spec
        (kupdate
          spec
          dependencies: (lines/s (s/map service-name (list->seq depends))))))))

(define (k/field-list proc)
  (kompose
    (k/map proc)
    (k/map list->seq)
    k/recur))

;; services->packages takes a list of services
;; and produces a complete list of packages
;; that the services depend upon, including
;; the necessary init scripts and binaries
(: services->packages ((list-of vector) -> (list-of procedure)))
(define (services->packages lst)
  (let* ((tail     ((s/packages lst) cons '()))
         (all      (list->seq lst))
         (k/users  (k/field-list service-users))
         (k/groups (k/field-list service-groups))
         (default  (make-service
                     name: 'default
                     spec: (%make-bundle
                             type: 'bundle
                             contents: (lines/s (s/map service-name all)))))
         (->art    (cut service->artifact-seq <> all))
         (db       (s6-rc-db (s/map ->art (s/cons* default all))))
         (users    (all (k/users cons) '()))
         (groups   (all (k/groups cons) '())))
    (append
      (cons db tail)
      (groups+users->artifacts groups users)
      (init-artifacts))))

(define *service-dir* "/run/service")
(define *catchall-fifo* "/run/service/s6-svscan-log/fifo")
(define *uncaught-logs* "/run/uncaught-logs")

;; init-script is the execline script for /init
(define (init-script)
  (let* ()
    (let-syntax ((fg* (syntax-rules ()
                           ((_ expr* ...)
                            (quasiquote
                              ((foreground (expr*)) ...))))))
    (execline*
      (export PATH "/usr/bin:/bin:/usr/sbin:/sbin")
      (export LC_ALL "C.UTF-8")
      (/bin/umask "022")
      ,@(fg*
          (echo "init starting")
          (mount -t proc -o "noexec,nosuid,nodev,hidepid=2" proc /proc)
          (mount -t sysfs -o "noexec,nosuid,nodev" sysfs /sys)
          ;; note: some kernels are configured to automagically mount /dev,
          ;; so no worries if this doesn't mount on its own
          (mount -t devtmpfs -o "exec,nosuid,mode=0755,size=2M" devtmpfs /dev)
          (mkdir -p /dev/pts)
          (mount -t devpts -o "gid=5,mode=0620,noexec,nosuid" devpts /dev/pts)
          (mkdir -p /dev/shm)
          (mount -t tmpfs -o "nodev,nosuid,noexec,size=5%,mode=1777" shm /dev/shm)
          (mount -t tmpfs -o "exec,nosuid,nodev,mode=0755" tmpfs /run)
          ;; to keep world-writeable /tmp from being a trivial DoS vector,
          ;; limit the size of the mount to some fixed % of RAM
          ;; TODO: make this configurable?
          (mount -t tmpfs -o "noexec,nosuid,nodev,mode=1777,size=10%" tmpfs /tmp))
      (if ((mkdir -p ,*service-dir*)))
      (if ((elglob svcs "/etc/early-services/*")
           (cp -r $svcs ,*service-dir*)))
      (foreground ((mkfifo -m "0600" ,*catchall-fifo*)))
      (if ((mkdir -p ,*uncaught-logs*)))
      (foreground ((chown catchlog:catchlog ,*uncaught-logs*)))
      (foreground ((chmod "2700" ,*uncaught-logs*)))
      (piperw 3 4)
      ;; we shouldn't run s6-rc-init until
      ;; s6-svscan has started, so we need
      ;; to do a little pipe juggling
      (background ((fdclose 4)
                   (fdmove 0 3)
                   (withstdinas ignored)
                   (redirfd -r 0 /dev/null)
                   (if ((s6-rc-init -c /etc/s6-rc/compiled
                                    -l /run/s6-rc
                                    ,*service-dir*)))
                   (s6-rc -v2 -u change default)))
      (fdclose 3)
      (foreground ((echo "beginning s6-svscan")))
      (redirfd -r 0 /dev/null)
      (redirfd -w -n -b 1 ,*catchall-fifo*)
      (fdmove -c 2 1)
      (s6-svscan -d 4 -St0 ,*service-dir*)))))

;; init-artifacts is the list of artifacts and package-lambdas
;; that are the contents of a working s6-rc init system,
;; absent the actual compiled service database
(define (init-artifacts)
  (let* ((init   (interned "/sbin/init" #o700 (with-output-to-string
                                                (lambda ()
                                                  (write-exexpr (init-script))))))
         (logger (interned "/etc/early-services/s6-svscan-log/run"
                           #o700
                           (with-output-to-string
                             (lambda ()
                               (write-exexpr
                                 (execline*
                                   (redirfd -w 2 /dev/console)
                                   (redirfd -r -n -b 0 fifo)
                                   (s6-setuidgid catchlog)
                                   (exec -c)
                                   (s6-log t /run/uncaught-logs))))))))
    (list init logger
          (interned-dir "/tmp" #o1777)
          (interned-dir "/dev" #o755)
          (interned-dir "/var" #o755)
          (interned-dir "/run" #o755)
          (interned-dir "/proc" #o555)
          (interned-dir "/sys" #o555)
          (interned-dir "/boot" #o755)
          s6 s6-rc execline-tools busybox-full)))

