(import
  scheme
  (only (chicken string) conc)
  (distill plan)
  (distill base)
  (distill package)
  (distill kvector))

(define libffi
  (let* ((ver '3.3)
         (src (remote-archive
                (conc "https://github.com/libffi/libffi/releases/download/v" ver "/libffi-" ver ".tar.gz")
                "D5qxr8A4qKcecQUxZOf_kYJNvwVv64my5oB_bnJyy7M=")))
    (lambda (conf)
      (make-package
        label:  (conc "libffi-" ver "-" ($arch conf))
        src:    src
        tools:  (cc-for-target conf)
        inputs: (list musl libssp-nonshared)
        build:  (gnu-recipe
                  (conc "libffi-" ver)
                  ($gnu-build (kwith
                                conf
                                configure-flags: (+= '(--disable-docs)))))))))
