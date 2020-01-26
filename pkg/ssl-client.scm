(import
  scheme
  (only (package) make-package)
  (only (plan)
        make-recipe
        fetch-remote-file!)
  (only (chicken string) conc)
  (only (base)
        cc-env
        cc-for-target
        musl
        libssp-nonshared)
  (execline)
  (pkg libressl))

;; TLS client for wget (shamelessly lifted from Alpine)
(define ssl-client
  (lambda (conf)
    (make-package
      label:  (conc "ssl-client-" (conf 'arch))
      src:    (fetch-remote-file!
                "https://raw.githubusercontent.com/alpinelinux/aports/bb3bc00f304cb4f0611d45555d124221d365bdce/main/busybox/ssl_client.c"
                "l6ULeIMMPl9KsPmufaCZH0W94xBheoS5k1u5a-56zUU="
                "/src/ssl_client.c"
                #o644)
      tools:  (cc-for-target conf)
      inputs: (list libressl musl libssp-nonshared)
      build:  (make-recipe
                script: (let* ((cenv   (cc-env conf))
                               (CC     (cdr (assq 'CC cenv)))
                               (CFLAGS (cdr (assq 'CFLAGS cenv))))
                          (execline*
                            (cd /src)
                            (if ((,CC ,@CFLAGS ssl_client.c -ltls -lssl -lcrypto -o ssl_client)))
                            (install -D -m "755" ssl_client /out/usr/bin/ssl_client)))))))

