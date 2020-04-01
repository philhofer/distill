(import
  scheme
  (only (chicken string) conc)
  (distill plan)
  (distill package)
  (distill kvector)
  (distill base)
  (pkg ncurses))

(define libreadline
  (let* ((ver '8.0)
         (src (remote-archive
                (conc "https://ftp.gnu.org/gnu/readline/readline-" ver ".tar.gz")
                "uIhQtdWX5EA75LisbuLAQyi1mcLYNydQCBEhEH3YcO4=")))
    (lambda (conf)
      (make-package
        label:  (conc "readline-" ver "-" ($arch conf))
        src:    src
        tools:  (cc-for-target conf)
        inputs: (list ncurses musl libssp-nonshared)
        build:  (gnu-recipe
                  (conc "readline-" ver)
                  ($gnu-build conf))))))
