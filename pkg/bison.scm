(import
  scheme
  (distill plan)
  (distill package)
  (distill kvector)
  (distill execline)
  (distill base))

(define bison
  (let ((src (source-template
               "bison" "3.5.2"
               "https://ftp.gnu.org/gnu/$name/$name-$version.tar.gz"
               "RzNc3Yv-A6hUD9trf_dAdZRKF_Lhvwmpw6r_UAtVyY0=")))
    (lambda (conf)
      (source->package
        conf
        src
        tools:  (append (list m4 perl) (cc-for-target conf))
        inputs: (list musl libssp-nonshared)
        build:  (gnu-recipe
                  ($gnu-build conf))))))
