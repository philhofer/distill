(module (pkg flex)
  (flex)
  (import
    scheme
    (base)
    (package)
    (plan)
    (only (chicken string) conc)
    (pkg bison))

  (define flex
    (let* ((version '2.6.4)
           (leaf    (remote-archive
                      (conc "https://github.com/westes/flex/releases/download/v" version "/flex-" version ".tar.gz")
                      "iyTO0NJ3ype1atS2SbQkeEA24JzbugW3OCxyRQVBBCw=")))
      (lambda (conf)
        (make-package
          #:label  (conc "flex-" version "-" (conf 'arch))
          #:src    leaf
          ;; NOTE: flex builds some binaries for the build system
          ;; as part of bootstrapping, so it may need 2 C toolchains
          #:tools  (append (list m4 bison musl) (cc-for-target conf))
          #:inputs (list musl)
          #:build  (gnu-build (conc "flex-" version) conf)))))

  )
