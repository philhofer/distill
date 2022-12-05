(define (perm? item)
  (and (list? item)
       (= (length item) 4)))

;; <platform> contains all of the hardware-specific bits
;; necessary to produce a functional disk image
(define-kvector-type
  <platform>
  make-platform
  platform?
  ;; config is a <config> that is used
  ;; to expand packages for this platform
  (platform-config    config:   #f  config?)
  ;; kernel is the kernel used for this platform,
  ;; or #f if this platform doesn't use one (containers, likely)
  (platform-kernel    kernel:   #f  (perhaps procedure?))
  ;; cmdline is the list of arguments passed to the kernel on boot
  (platform-cmdline   cmdline:  '() (list-of (disjoin symbol? string?)))
  ;; packages is a list of packages that are
  ;; mandatory for this platform
  (platform-packages packages: '() (list-of (disjoin procedure? artifact?)))
  ;; services is a list of services that are
  ;; mandatory for this platform;
  ;; services *must* provide at least:
  ;;  - read-write /var mount, ideally on persistent media
  ;;  - login or debug services, if relevant/needed (for example, serial console login)
  (platform-services services: '() (list-of service?))
  ;; users is the list of users that
  ;; are defined for the platform
  ;; note: platform users must be between uid 1 and uid 98
  (platform-users    users:    '() (list-of user?))
  ;; groups is the list of groups
  ;; that are defined for the platform
  ;; note: platform groups must be between gid 1 and gid 98
  (platform-groups   groups:   '() (list-of group?))
  ;; perms is the list of filesystem ownership and permissions
  ;; overrides; these should be only what are necessary
  ;; to make the packages and services for the platform
  ;; work correctly
  ;; (also, note that these only apply to the root filesystem!)
  (platform-perms    perms:    '() (list-of perm?))
  ;; mkimage is a function that takes a <platform>
  ;; and produces an image package-lambda, or #f if no such concept
  ;; is relevant to the platform (XXX when is that ever the case?)
  (platform-mkimage  mkimage:   #f (perhaps procedure?)))

(define-kvector-type
  <system>
  make-system
  system?
  ;; services is the list of services
  ;; that are built into the system
  (system-services services: '() (list-of service?))
  ;; packages is the list of packages
  ;; that are installed into the system
  ;; independently of any service requirements
  (system-packages packages: '() (list-of (disjoin procedure? artifact?)))
  ;; users is the list of users created
  ;; for the system
  ;; note: system users should begin at uid 1000
  (system-users    users:    '() (list-of user?))
  ;; groups is the list of groups created
  ;; for the system
  ;; note: system groups should begin at gid 1000
  (system-groups   groups:   '() (list-of group?))
  ;; perms is the list of filesystem ownership
  ;; and permissions overrides
  ;; note: these only apply to the root filesystem,
  ;; and not any additional mountpoints
  ;;
  ;; by convention, directories that need to be modified
  ;; should have a trailing / character
  (system-perms    perms:    '() (list-of perm?)))

;; unpack-filemodes takes a list of filemode specs like
;; '((name mode user group) ...)
;; and looks up user and group numbers when they
;; are symbols rather than raw uids and gids
(define (unpack-filemodes lst users groups)
  (let* ((lookup (lambda (id from-field to-field lst)
                   (cond
                    ((integer? id) id)
                    ((symbol? id)
                     (let loop ((lst lst))
                       (cond
                        ((null? lst) (error "unknown id" id))
                        ((eq? (from-field (car lst)) id)
                         (to-field (car lst)))
                        (else (loop (cdr lst))))))
                    (else (error "cannot lookup" id)))))
         (get-user (lambda (id)
                     (lookup id user-name user-uid (append users base-users))))
         (get-group (lambda (id)
                      (lookup id group-name group-gid (append groups base-groups)))))
    (map
     (lambda (spec)
       (let ((fullpath (car spec))
             (mode     (cadr spec))
             (user     (caddr spec))
             (group    (cadddr spec)))
         (list fullpath mode (get-user user) (get-group group))))
     lst)))

(define (platform+system->plan plat sys)
  (letrec ((union/eq?  (lambda (a b)
                         (cond
                          ((null? a) b)
                          ((null? b) a)
                          (else
                           (let ((fb (car b)))
                             (union/eq?
                              (if (memq fb a) a (cons fb a))
                              (cdr b))))))))
    (let* ((pkgs     (union/eq? (platform-packages plat)
                                (system-packages sys)))
           (svcs     (union/eq? (platform-services plat)
                                (system-services sys)))
           (users    (append (platform-users plat)
                             (system-users sys)))
           (groups   (append (platform-groups plat)
                             (system-groups sys)))
           (perms    (unpack-filemodes
                      (append (platform-perms plat) (system-perms sys))
                      users groups))
           (newimg   (platform-mkimage plat))
           (expand   (expander (platform-config plat)))
           (rootpkgs (union/eq? (services->packages
                                 services: svcs
                                 extra-users: users
                                 extra-groups: groups
                                 ;; for now just assume no kernel means we're in a container
                                 container?: (not (platform-kernel plat))) pkgs)))
      (expand (newimg plat rootpkgs perms)))))
