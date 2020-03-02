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
  (let* ((version '3.4.2)
         (leaf    (remote-archive
                    (conc "https://ftp.gnu.org/gnu/bison/bison-" version ".tar.xz")
                    "mISdVqFsbf8eeMe8BryH2Zf0oxcABOyW26NZT61RMSU=")))
    (lambda (conf)
      (make-package
        label:  (conc "bison-" version "-" ($arch conf))
        src:    leaf
        tools:  (append (list m4 perl) (cc-for-target conf))
        inputs: (list musl libssp-nonshared)
        build:  (gnu-recipe
                  (conc "bison-" version)
                  (kwith
                    ($gnu-build conf)
                    ;; there is a buggy makefile in examples/ that will
                    ;; occasionally explode during parallel builds;
                    ;; just delete the directory entirely
                    ;; https://lists.gnu.org/archive/html/bug-bison/2019-10/msg00044.html
                    pre-configure: (+= (execline*
                                         (if ((rm -rf examples/c/recalc)))))))))))
