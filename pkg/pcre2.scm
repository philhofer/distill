(import
  scheme
  (distill plan)
  (distill package)
  (distill kvector)
  (distill base)
  (pkg libedit))

(define pcre2
  (let ((src (source-template
               "pcre2" "10.34"
               "https://ftp.pcre.org/pub/pcre/$name-$version.tar.gz"
               "dUEXxoMgBjr2H6OHV8jRk-dWOqW-XqOnNeFlYZ3mp3A=")))
    (lambda (conf)
      (source->package
        conf
        src
        tools:  (cc-for-target conf)
        inputs: (list libedit zlib bzip2 musl libssp-nonshared)
        build:  (gnu-recipe
                  (kwith
                    ($gnu-build conf)
                    configure-args: (+= '(--disable-dependency-tracking
                                           --enable-pcre2-16
                                           --enable-pcre2-32
                                           --enable-pcre2grep-libz
                                           --enable-pcre2grep-libbz2
                                           --with-match-limit-depth=8192))))))))
