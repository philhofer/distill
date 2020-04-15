(import
  scheme
  (only (chicken string) conc)
  (distill plan)
  (distill package)
  (distill base)
  (pkg ncurses))

(define libedit
  (let ((src (source-template
               "libedit" "20191231-3.1"
               "https://www.thrysoee.dk/editline/$name-$version.tar.gz"
               "x0NyCF6katZwtz3Wpc8fVKjK8nawKLWVgsdZ2eTgFsU=")))
    (lambda (conf)
      (source->package
        conf
        src
        tools:  (cc-for-target conf)
        inputs: (list ncurses musl libssp-nonshared)
        build:  (gnu-recipe ($gnu-build conf))))))
