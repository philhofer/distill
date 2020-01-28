(import
  scheme
  (plan)
  (base)
  (package)
  (only (chicken string) conc))

(define xz-utils
  (let* ((version '5.2.4)
         (src     (remote-archive
                    (conc "https://tukaani.org/xz/xz-" version ".tar.xz")
                    "xbmRDrGbBvg_6BxpAPsQGBrFFAgpb0FrU3Yu1zOf4k8=")))
    (lambda (conf)
      (make-package
        label:  (conc "xz-utils-" (conf 'arch))
        src:    src
        tools:  (cc-for-target conf)
        inputs: (list musl libssp-nonshared)
        build:  (gnu-build
                  (conc "xz-" version) conf)))))
