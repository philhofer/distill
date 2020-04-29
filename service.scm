(define symbolic?       (perhaps symbol?))
(define maybe-int?      (perhaps integer?))
(define maybe-string?   (perhaps string?))
(define maybe-symbolic? (perhaps symbolic?))

;; note: field names for <longrun>, <oneshot>, and <bundle>
;; correspond precisely to the file names that s6-rc-compile
;; expects to be present in a service definition directory

(define-kvector-type
  <longrun>
  longrun*
  longrun?
  (type:   'longrun (eq?/c 'longrun))
  (run:    #f       list?)
  (finish: #f       (perhaps list?))
  (pipeline-name:   #f maybe-symbolic?)
  (nosetsid:        #f maybe-symbolic?)
  (max-death-tally: #f maybe-int?)
  (down-signal:     #f maybe-int?)
  (timeout-up:      #f maybe-int?)
  (timeout-down:    #f maybe-int?)
  (timeout-kill:    #f maybe-int?)
  (timeout-finish:  #f maybe-int?)
  (dependencies:    #f maybe-string?)
  (notification-fd: #f maybe-int?)
  (producer-for:    #f maybe-symbolic?)
  (consumer-for:    #f maybe-symbolic?))

(define-kvector-type
  <oneshot>
  oneshot*
  oneshot?
  (type: 'oneshot (eq?/c 'oneshot))
  (up:   #f       list?)
  (down: #f       (perhaps list?))
  (timeout-up:   #f maybe-int?)
  (timeout-down: #f maybe-int?)
  (dependencies: #f maybe-string?))

(define-kvector-type
  <bundle>
  %make-bundle
  bundle?
  (type: 'bundle (eq?/c 'bundle))
  (contents: #f string?))


(: bundle* (#!rest vector --> vector))
(define (bundle* . args)
  (%make-bundle
    contents: (lines/s (s/bind (list->seq args) service-name))))

(: spec? (* -> boolean))
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
          dir:    "/"
          tools:  (list busybox-core execline-tools s6 s6-rc)
          inputs: '()
          build:  `((if ((mkdir -p "/out/etc/s6-rc")))
                    (s6-rc-compile "/out/etc/s6-rc/compiled" "/src/services")))))))

(define-kvector-type
  <service>
  make-service
  service?
  (service-name   name:   #f  symbol?)
  (service-inputs inputs: '() (list-of (or/c procedure? artifact?)))
  (service-users  users:  '() (list-of procedure?))
  (service-groups groups: '() (list-of procedure?))
  (service-after  after:  '() (list-of (or/c procedure? string? symbol?)))
  (service-spec   spec:   #f  spec?))

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
    `((export PATH "/usr/bin:/bin:/usr/sbin:/sbin")
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
      ;; elglob won't match leading '.' characters,
      ;; so we have to use an extra glob to make sure
      ;; we pick up stuff in .s6-svscan
      (if ((elglob -m svcs "/etc/early-services/*")
           (elglob -m dots "/etc/early-services/.[!.]*")
           (cp -r $dots $svcs ,*service-dir*)))
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
  (let* ((script*  (lambda (path body)
                     (interned path #o744
                               (with-output-to-string
                                 (lambda () (write-exexpr body))))))
         (scanctl* (lambda (script flag)
                     (script* script `((foreground ((s6-rc -v2 -bad -t 10000 change)))
                                       (s6-svscanctl ,flag /run/service)))))
         (init     (script* "/sbin/init" (init-script)))
         (reboot   (scanctl* "/sbin/reboot" "-i"))
         (poweroff (scanctl* "/sbin/poweroff" "-7"))
         (halt     (scanctl* "/sbin/halt" "-0"))
         (crash    (script* "/etc/early-services/.s6-svscan/crash"
                            '((foreground ((redirfd -w 1 /dev/console)
                                           (fdmove -c 2 1)
                                           (echo "pid 1 is crashing!")))
                              (foreground ((kill -15 -1)))
                              (foreground ((sleep 1)))
                              (foreground ((kill -9 -1)))
                              (foreground ((sleep 1)))
                              (foreground ((sync)))
                              (hard reboot))))
         (finish   (interned "/etc/early-services/.s6-svscan/finish"
                             #o744
                             (with-output-to-string
                               (lambda ()
                                 (write-exexpr
                                   ;; note: at this point it is expected that
                                   ;; everything under process supervision is already dead,
                                   ;; but there still may be other processes running
                                   '((redirfd -w 1 /dev/console)
                                     (fdmove -c 2 1)
                                     (foreground ((kill -15 -1)))
                                     (foreground ((sleep 1)))
                                     (foreground ((kill -9 -1)))
                                     (foreground ((sync)))
                                     (hard $1))
                                   shebang: "#!/bin/execlineb -s1")))))
         (logger   (interned "/etc/early-services/s6-svscan-log/run"
                             #o744
                             (with-output-to-string
                               (lambda ()
                                 (write-exexpr
                                   `((redirfd -w 2 /dev/console)
                                     (redirfd -r -n -b 0 fifo)
                                     (s6-setuidgid catchlog)
                                     (exec -c)
                                     (s6-log t /run/uncaught-logs))))))))
    (list init logger finish crash reboot poweroff halt
          (interned-dir "/tmp" #o1777)
          (interned-dir "/dev" #o755)
          (interned-dir "/var" #o755)
          (interned-dir "/run" #o755)
          (interned-dir "/proc" #o555)
          (interned-dir "/sys" #o555)
          (interned-dir "/boot" #o755)
	  ;; TODO: vendor these:
	  (remote-file
	   "https://salsa.debian.org/md/netbase/-/raw/master/etc/services"
	   "nw1h7RChZb_XKCID72w_9sFUXE3Nluej6ZC9TWEuOq8="
	   "/etc/services" #o644)
	  (remote-file
	   "https://salsa.debian.org/md/netbase/-/raw/master/etc/protocols"
	   "gykO3waWpzCvDqGEFidDstWolF4YoIjMyvOkG16y1b4="
	   "/etc/protocols" #o644)
          s6 s6-rc execline-tools busybox-full hard)))
