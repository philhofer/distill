
(define-type svc-params (list-of (pair keyword *)))
(define-type s6-svc (pair (or string symbol) (list-of svc-params)))

(define (s6-svc name args)
  (cons name
        (let loop ((args args))
          (if (null? args)
            '()
            (let ((key (car args))
                  (val (cadr args)))
              (unless (keyword? key)
                (error "expected keyword; found" key))
              (cons (cons key val) (loop (cddr args))))))))

;; spec->artifact-seq takes an s6-rc service specification
;; and yields a sequence of artifacts that need to be present
;; for s6-rc-compile to produce the correct s6-rc database entry
;;
;; e.g. (spec->artifact-seq (s6-svc "foo" up: ... down: ...))
;; produces artifacts for /src/services/foo/up, etc.
(define (spec->artifact-seq spec)
  (let* ((dir        (car spec))
         (spec       (cdr spec))
         (pair->file (lambda (p)
                       (let ((file (keyword->string (car p)))
                             (val  (cdr p)))
                         (interned
                           (filepath-join "/src/services" dir file)
                           #o644
                           (with-output-to-string
                             (lambda () (if (pair? val)
                                          (write-exexpr val)
                                          (begin (display val) (newline)))))))))
         (type       (assq type: spec))
         (check      (lambda (yes no)
                       (for-each
                         (lambda (y)
                           (unless (assq y spec)
                             (fatal "expected spec" y "in service" dir spec)))
                         yes)
                       (for-each
                         (lambda (n)
                           (when (assq n spec)
                             (fatal "unexpected spec" n "in service" dir spec)))
                         no))))
      (if type
        ;; basic sanity check on service specs so that
        ;; you don't have to go spelunking through logs
        ;; to figure out why s6-rc-compile failed
        (case (cdr type)
          ((oneshot)
           (check '(up:) '(run: finish:)))
          ((longrun)
           (check '(run:) '(up: down:)))
          ((bundle)
           (check '(contents:) '(dependencies: up: down: run: finish:)))
          (else
            (fatal dir "has an unrecognized service type:" (cdr type))))
        (fatal dir "does not have 'type:' in spec"))
    (s/map pair->file (list->seq spec))))

;; s6-rc-db converts a sequence of s6-rc package specifications
;; and produces a package-lambda for the compiled service database
(define s6-rc-db
  (let* ((k/spec->art    (kompose
                           (k/map spec->artifact-seq)
                           k/recur))
         (cons-artifacts (k/spec->art cons)))
    (lambda (spec-seq)
      (let ((artifacts (spec-seq cons-artifacts '())))
        (lambda (conf)
          (make-package
            label:  "s6-rc-db"
            src:    artifacts
            tools:  (list busybox-core execline-tools s6 s6-rc)
            inputs: '()
            build:  (make-recipe
                      script: (execline*
                                (if ((mkdir -p "/out/etc/s6-rc")))
                                (s6-rc-compile "/out/etc/s6-rc/compiled" "/src/services")))))))))

(define s6
  (let* ((version '2.9.0.1)
         (src     (remote-archive
                    (conc "https://skarnet.org/software/s6/s6-" version ".tar.gz")
                    "uwnwdcxfc7i3LTjeNPcnouhShXzpMPIG0I2AbQfjL_I=")))
    (lambda (conf)
      (make-package
        label:  (conc "s6-" (conf 'arch))
        src:    src
        tools:  (cc-for-target conf)
        inputs: (list skalibs execline-tools musl libssp-nonshared)
        build:  (ska-build (conc "s6-" version) conf
                           extra-configure: `(,(conc '--with-sysdeps= (sysroot conf) "/lib/skalibs/sysdeps")
                                               --enable-static-libc))))))
(define s6-rc
  (let* ((version '0.5.1.1)
         (src     (remote-archive
                    (conc "https://skarnet.org/software/s6-rc/s6-rc-" version ".tar.gz")
                    "KCvqdFSKEUNPkHQuCBfs86zE9JvLrNHU2ZhEzLYY5RY=")))
    (lambda (conf)
      (make-package
        label:  (conc "s6-rc-" (conf 'arch))
        src:    src
        tools:  (cc-for-target conf)
        inputs: (list s6 skalibs execline-tools musl libssp-nonshared)
        build:  (ska-build (conc "s6-rc-" version) conf
                           extra-configure: `(,(conc '--with-sysdeps= (sysroot conf) "/lib/skalibs/sysdeps")
                                               --enable-static-libc))))))

(define busybox-full
  (busybox/config
    "dqQ64qnqMRlFufvFWDpkBeEHAVfmWy_ivBhy6FAP-6Y="
    (list linux-headers)))

;; service represents a system service
(defstruct service
  (name : symbol)
  ;; inputs is a list of artifacts and package-lambdas;
  ;; these represent filesystem componenents that must
  ;; be present at runtime
  ((inputs '()) : list)
  ;; users that need to exist
  ((users '()) : list)
  ;; groups that need to exist
  ((groups '()) : list)
  ;; 'after' is a list of functions (predicates)
  ;; applied to other services to determine
  ;; if they must be started before this one
  ((after '()) : (list-of procedure))
  ;; specification for the s6-rc service
  (spec : list))

(: s/packages ((list-of (struct service)) -> procedure))
(define (s/packages lst)
  (s/bind (list->seq lst)
          (kompose
            (k/map service-inputs)
            (k/map list->seq)
            k/recur
            (k/uniq test: eq? hash: eq?-hash))))

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

(: service->s6-svc ((struct service) procedure -> list))
(define (service->s6-svc svc svc-seq)
  (let ((depends (service-depends svc svc-seq)))
    (s6-svc
      (service-name svc)
      (if (null? depends)
        (service-spec svc)
        (append (list dependencies: (lines/s (list->seq depends)))
                (service-spec svc))))))

(define (k/field-list proc)
  (kompose
    (k/map proc)
    (k/map list->seq)
    k/recur))

;; the 'default' runlevel is the one used at boot
(define (default-runlevel svcs)
  (make-service
    name: 'default
    spec: `(type: bundle
            contents: ,(lines/s (s/bind svcs (k/map service-name))))))

;; services->packages takes a list of services
;; and produces a complete list of packages
;; that the services depend upon, including
;; the necessary init scripts and binaries
(: services->packages ((list-of (struct service)) -> (list-of procedure)))
(define (services->packages lst)
  (let* ((tail     ((s/packages lst) cons '()))
         (all      (list->seq lst))
         (k/users  (k/field-list service-users))
         (k/groups (k/field-list service-groups))
         (default  (default-runlevel all))
         (->svc    (cut service->s6-svc <> all))
         (db       (s6-rc-db (s/map ->svc (s/cons* default all))))
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
      (redirfd -wnb 1 ,*catchall-fifo*)
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
                                   (redirfd -rnb 0 fifo)
                                   (s6-setuidgid catchlog)
                                   (exec -c)
                                   (s6-log t /run/uncaught-logs)))))))
         (finish (interned "/etc/early-services/.s6-svscan/finish"
                           #o700
                           (with-output-to-string
                             (lambda ()
                               (write-exexpr
                                 (execline*
                                   (s6-rc -bad change)
                                   (s6-svc -X -- /run/service/s6-svscan-log)
                                   (reboot))))))))

    (list init logger finish
          (interned-dir "/tmp" #o1777)
          (interned-dir "/dev" #o755)
          (interned-dir "/var" #o755)
          (interned-dir "/run" #o755)
          (interned-dir "/proc" #o555)
          (interned-dir "/sys" #o555)
          (interned-dir "/boot" #o755)
          s6 s6-rc execline-tools busybox-full)))

