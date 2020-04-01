(import
  scheme
  (only (chicken string) conc)
  (distill plan)
  (distill package)
  (distill kvector)
  (distill base))

(define libexpat
  (let* ((ver '2.2.9)
         (tag "R_2_2_9")
         (src (remote-archive
                (conc "https://github.com/libexpat/libexpat/releases/download/" tag "/expat-" ver ".tar.gz")
                "KQQEiL5DQZGhXJwZR8ETCf5wfR6l5MP6am4PEAHhkn0=")))
    (lambda (conf)
      (make-package
        label:  (conc "libexpat-" ver "-" ($arch conf))
        src:    src
        tools:  (cc-for-target conf)
        inputs: (list musl libssp-nonshared)
        build:  (gnu-recipe
                  (conc "expat-" ver)
                  ($gnu-build
                    (kwith conf
                           configure-flags: (+= '(--without-examples
                                                   --without-tests
                                                   --with-getrandom
                                                   --without-docbook)))))))))
