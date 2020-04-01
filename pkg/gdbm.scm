(import
  scheme
  (only (chicken string) conc)
  (distill plan)
  (distill package)
  (distill kvector)
  (distill base))

(define gdbm
  (let* ((ver '1.18.1)
         (src (remote-archive
                (conc "https://ftp.gnu.org/gnu/gdbm/gdbm-" ver ".tar.gz")
                "7ZcU9Pwu1_5MUbjqtWKn2bMgkx05UMTmzv9LqCjJtmg=")))
    (lambda (conf)
      (make-package
        label:  (conc "gdbm-" ver "-" ($arch conf))
        src:    src
        tools:  (cc-for-target conf)
        inputs: (list musl libssp-nonshared)
        build:  (gnu-recipe
                  (conc "gdbm-" ver)
                  ($gnu-build
                    (kwith
                      conf
                      configure-flags: (+= '(--disable-dependency-tracking --enable-fast-install)))))))))
