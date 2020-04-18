(import
  scheme
  (distill plan)
  (distill package)
  (distill kvector)
  (distill base))

(define gdbm
  (let ((src (source-template
               "gdbm" "1.18.1"
               "https://ftp.gnu.org/gnu/$name/$name-$version.tar.gz"
               "7ZcU9Pwu1_5MUbjqtWKn2bMgkx05UMTmzv9LqCjJtmg=")))
    (lambda (conf)
      (source->package
        conf
        src
        tools:  (cc-for-target conf)
        inputs: (list musl libssp-nonshared)
        build:  (gnu-recipe
		 (kwith
                  ($gnu-build conf)
                  configure-args: (+= '(--disable-dependency-tracking --enable-fast-install))))))))
