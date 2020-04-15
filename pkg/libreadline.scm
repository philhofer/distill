(import
  scheme
  (distill plan)
  (distill package)
  (distill kvector)
  (distill base)
  (pkg ncurses))

(define libreadline
  (let ((src (source-template
               "readline" "8.0"
               "https://ftp.gnu.org/gnu/$name/$name-$version.tar.gz"
               "uIhQtdWX5EA75LisbuLAQyi1mcLYNydQCBEhEH3YcO4=")))
    (lambda (conf)
      (source->package
        conf
        src
        tools:  (cc-for-target conf)
        inputs: (list ncurses musl libssp-nonshared)
        build:  (gnu-recipe
                  ($gnu-build conf))))))
