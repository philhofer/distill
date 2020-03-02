(import
  scheme
  (distill base)
  (distill plan)
  (distill package)
  (distill kvector)
  (distill execline)
  (only (chicken string) conc))

(define tar
  (let* ((version '1.32)
         (src     (remote-archive
                    (conc "https://ftp.gnu.org/gnu/tar/tar-"
                          version
                          ".tar.xz")
                    "yevL-uqj4F98n_dVcnVzCc6F6jEGPn2IqXZqH28Y3Go=")))
    (lambda (conf)
      (make-package
        label:  (conc "tar-" ($arch conf))
        src:    src
        tools:  (cc-for-target conf)
        inputs: (list musl libssp-nonshared)
        build:  (gnu-recipe
                  (conc "tar-" version)
                  (kwith
                    ($gnu-build conf)
                    exports: (+= '((gl_cv_func_gettimeofday_clobber . no)
                                   (gl_cv_func_tzset_clobber . no)))))))))
