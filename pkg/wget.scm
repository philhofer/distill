(import
  scheme
  (only (chicken string) conc)
  (distill plan)
  (distill kvector)
  (distill package)
  (distill base))

(define wget
  (let* ((ver '1.20.3)
         (src (remote-archive
                (conc "https://ftp.gnu.org/gnu/wget/wget-" ver ".tar.gz")
                "G3jWdBQ-_4BxoX6NpCl6-mbpV_NH0BEOZZRMLKqlTrs=")))
    (lambda (conf)
      (make-package
        label:  (conc "wget-" ver "-" ($arch conf))
        src:    src
        tools:  (cons perl (cc-for-target conf))
        inputs: (list libressl musl libssp-nonshared)
        build:  (gnu-recipe
                  (conc "wget-" ver)
                  (kwith
                    ($gnu-build conf)
                    pre-configure: (+= '((export OPENSSL_LIBS "-lssl -lcrypto")
                                         (export OPENSSL_CFLAGS -lssl)))
                    configure-args: (+= '(--disable-dependency-tracking
                                           --with-ssl=openssl
                                           --with-openssl
                                           --without-libidn))))))))
