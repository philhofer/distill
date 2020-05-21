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
  ;; mkimage is a function that takes a <platform>
  ;; and produces an image package-lambda, or #f if no such concept
  ;; is relevant to the platform (XXX when is that ever the case?)
  (platform-mkimage  mkimage:   #f (perhaps procedure?)))

(define-kvector-type
  <system>
  make-system
  system?
  (system-services services: '() (list-of service?))
  (system-packages packages: '() list?))

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
    (let ((pkgs (union/eq? (platform-packages plat)
			   (system-packages sys)))
	  (svcs (union/eq? (platform-services plat)
			   (system-services sys)))
	  (newimg (platform-mkimage plat))
	  (expand (expander (platform-config plat))))
      (expand (newimg plat (union/eq? (services->packages svcs) pkgs))))))
