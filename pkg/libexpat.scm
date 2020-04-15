(import
  scheme
  (only (chicken string) string-translate* string-intersperse string-split)
  (distill plan)
  (distill package)
  (distill kvector)
  (distill base))

(define libexpat
  (let* ((ver "2.2.9")
         ;; tag for 2.2.9 is R_2_2_9
         (tag (string-intersperse
                (cons "R" (string-split ver ".")) "_"))
         (url (string-translate*
                "https://github.com/libexpat/libexpat/releases/download/$tag/$name-$version.tar.gz"
                `(("$tag" . ,tag))))

         (src (source-template
                "expat" "2.2.9" url
                "KQQEiL5DQZGhXJwZR8ETCf5wfR6l5MP6am4PEAHhkn0=")))
    (lambda (conf)
      (source->package
        conf
        src
        tools:  (cc-for-target conf)
        inputs: (list musl libssp-nonshared)
        build:  (gnu-recipe
                  ($gnu-build
                    (kwith conf
                           configure-flags: (+= '(--without-examples
                                                   --without-tests
                                                   --with-getrandom
                                                   --without-docbook)))))))))
