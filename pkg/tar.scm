(import
  scheme
  (distill plan)
  (distill package)
  (distill kvector)
  (distill base))

(define tar
  (let ((src (source-template
               "tar" "1.32"
               "https://ftp.gnu.org/gnu/$name/$name-$version.tar.xz"
               "yevL-uqj4F98n_dVcnVzCc6F6jEGPn2IqXZqH28Y3Go=")))
    (lambda (conf)
      (source->package
        conf
        src
        tools:  (cc-for-target conf)
        inputs: (list musl libssp-nonshared)
        build:  (gnu-recipe
                  (kwith
                    ($gnu-build conf)
                    exports: (+= '((gl_cv_func_gettimeofday_clobber . no)
                                   (gl_cv_func_tzset_clobber . no)))))))))
