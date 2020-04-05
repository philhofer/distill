(import
  scheme
  (only (chicken string) conc)
  (distill plan)
  (distill package)
  (distill kvector)
  (distill base))

(define curl
  (let* ((ver '7.69.1)
         (src (remote-archive
                (conc "https://curl.haxx.se/download/curl-" ver ".tar.xz")
                "SE9rx8lnJG7UyJMz8kbJ8affBfFBp4YCMwLwNB_1ErE=")))
    (lambda (conf)
      (make-package
        label:  (conc "curl-" ver "-" ($arch conf))
        src:    src
        tools:  (cons perl (cc-for-target conf))
        inputs: (list libressl zlib musl libssp-nonshared)
        build:  (gnu-recipe
                  (conc "curl-" ver)
                  (kwith
                    ($gnu-build conf)
                    configure-args: (+= '(--without-libidn
                                           --without-libidn2
                                           --without-nghttp2
                                           --disable-ldap
                                           --without-libssh2
                                           --enable-ipv6
                                           --enable-unix-sockets))))))))
