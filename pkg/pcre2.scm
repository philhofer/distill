(import
  scheme
  (only (chicken string) conc)
  (distill plan)
  (distill package)
  (distill kvector)
  (distill base)
  (pkg libedit))

(define pcre2
  (let* ((ver '10.34)
         (src (remote-archive
                (conc "https://ftp.pcre.org/pub/pcre/pcre2-" ver ".tar.gz")
                "dUEXxoMgBjr2H6OHV8jRk-dWOqW-XqOnNeFlYZ3mp3A=")))
    (lambda (conf)
      (make-package
        label:  (conc "pcre2-" ver "-" ($arch conf))
        src:    src
        tools:  (cc-for-target conf)
        inputs: (list libedit zlib bzip2 musl libssp-nonshared)
        build:  (gnu-recipe
                  (conc "pcre2-" ver)
                  (kwith
                    ($gnu-build conf)
                    configure-args: (+= '(--disable-dependency-tracking
                                          --enable-pcre2-16
                                          --enable-pcre2-32
                                          --enable-pcre2grep-libz
                                          --enable-pcre2grep-libbz2
                                          --with-match-limit-depth=8192))))))))
