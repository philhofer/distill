(import
  scheme
  (only (chicken load) require)
  (only (chicken string) conc)
  (distill plan)
  (distill package)
  (distill kvector)
  (distill base)
  (distill execline)
  (only (distill linux) perl))

(define bison
  (let* ((version '3.5.2)
         (leaf    (remote-archive
                    (conc "https://ftp.gnu.org/gnu/bison/bison-" version ".tar.gz")
                    "RzNc3Yv-A6hUD9trf_dAdZRKF_Lhvwmpw6r_UAtVyY0=")))
    (lambda (conf)
      (make-package
        label:  (conc "bison-" version "-" ($arch conf))
        src:    leaf
        tools:  (append (list m4 perl) (cc-for-target conf))
        inputs: (list musl libssp-nonshared)
        build:  (gnu-recipe
                  (conc "bison-" version)
                  (kwith
                    ($gnu-build conf)))))))
