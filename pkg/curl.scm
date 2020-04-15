(import
  scheme
  (distill plan)
  (distill package)
  (distill kvector)
  (distill base))

(define curl
  (let ((src (source-template
               "curl" "7.69.1"
               "https://curl.haxx.se/download/$name-$version.tar.xz"
               "SE9rx8lnJG7UyJMz8kbJ8affBfFBp4YCMwLwNB_1ErE=")))
    (lambda (conf)
      (source->package
        conf
        src
        tools:  (cons perl (cc-for-target conf))
        inputs: (list libressl zlib musl libssp-nonshared)
        build:  (gnu-recipe
                  (kwith
                    ($gnu-build conf)
                    configure-args: (+= '(--without-libidn
                                           --without-libidn2
                                           --without-nghttp2
                                           --disable-ldap
                                           --without-libssh2
                                           --enable-ipv6
                                           --enable-unix-sockets))))))))
