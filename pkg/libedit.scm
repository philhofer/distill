(import
  scheme
  (distill plan)
  (distill package)
  (distill base)
  (only (chicken string) conc)
  (pkg ncurses))

(define libedit
  (let* ((version '20191231-3.1)
         (src     (remote-archive
                    (conc "https://www.thrysoee.dk/editline/libedit-" version ".tar.gz")
                    "x0NyCF6katZwtz3Wpc8fVKjK8nawKLWVgsdZ2eTgFsU=")))
    (lambda (conf)
      (make-package
        label:  (conc "libedit-" ($arch conf))
        src:    src
        tools:  (cc-for-target conf)
        inputs: (list ncurses musl libssp-nonshared)
        build:  (gnu-recipe (conc "libedit-" version) ($gnu-build conf))))))
