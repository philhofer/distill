(import
  scheme
  (only (chicken string) conc)
  (distill plan)
  (distill kvector)
  (distill package)
  (distill base))

(define wget
  (let ((src (source-template
               "wget" "1.20.3"
               "https://ftp.gnu.org/gnu/$name/$name-$version.tar.gz"
               "G3jWdBQ-_4BxoX6NpCl6-mbpV_NH0BEOZZRMLKqlTrs=")))
    (lambda (conf)
      (source->package
        conf
        src
	env:    '((OPENSSL_LIBS . "-lssl -lcrypto")
		  (OPENSSL_CFLAGS . -lssl))
        tools:  (cons perl (cc-for-target conf))
        inputs: (list libressl musl libssp-nonshared)
        build:  (gnu-recipe
                  (kwith
                    ($gnu-build conf)
                    configure-args: (+= '(--disable-dependency-tracking
                                          --with-ssl=openssl
                                          --with-openssl
                                          --without-libidn))))))))
