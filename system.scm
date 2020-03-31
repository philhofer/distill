;; write-dd-script writes an execlineb script
;; that takes the destination device as the first argument
;; and dd's each given artifact to the device's partitions
;; in sequence
;; (#f indicates the partition should be skipped)
(define (write-setparts-script lst)
  (write-exexpr
    `((setparts "${1}" ,@(map
                           (lambda (x)
                             (if x (abspath (artifact-path x)) "-"))
                           lst)))
    shebang: "#!/bin/execlineb -s1"))

(define <system>
  (make-kvector-type
    services:
    packages:))

(define %make-system
  (kvector-constructor
    <system>
    services: '() (list-of service?)
    packages: '() (list-of procedure?)))

;; build-system takes a platform and a set of keyword+value arguments
;; and runs the platform's build function
;;
;; recognized keywords are:
;;   services: a list of system services (required)
;;   packages: an additional list of packages to be installed (optional)
;;
(define build-system
  (let ((services   (kvector-getter <system> services:))
        (packages   (kvector-getter <system> packages:)))
    (lambda (plat . args)
      (let* ((sys  (apply %make-system args))
             (svcs (services sys))
             (pkgs (packages sys)))
        ;; XXX need to dedup pkgs from svcs
        (plat (append (services->packages svcs) pkgs))))))

;; uniq-setparts-script produces a uniquely-named setparts script
(define (uniq-setparts-script prefix parts)
  ;; artifacts that become raw disk partitions
  ;; should be raw files, not archives, symlinks, etc.
  (for-each
    (lambda (art)
      (unless (eq? (artifact-kind art) 'file)
        (info "warning: artifact" (short-hash (artifact-hash art)) "is of kind" (artifact-kind art))))
    parts)
  (let* ((h (apply string-append (map artifact-hash parts)))
         (f (string-append prefix (substring h 0 10))))
    (info "writing final privileged script to" f)
    (with-output-to-file
      f
      (lambda () (write-setparts-script parts)))
    (set-file-permissions! f #o755)))
