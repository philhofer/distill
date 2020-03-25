(import
  scheme
  (only (distill package)
        make-package
        make-recipe
        $arch $CC $CFLAGS)
  (only (distill plan) remote-file)
  (only (chicken string) conc)
  (only (distill base)
        cc-for-target
        musl
        libssp-nonshared)
  (only (distill image) libressl))

;; TLS client for wget (shamelessly lifted from Alpine)
(define ssl-client
  (lambda (conf)
    (make-package
      label:  (conc "ssl-client-" ($arch conf))
      src:    (remote-file
                "https://raw.githubusercontent.com/alpinelinux/aports/bb3bc00f304cb4f0611d45555d124221d365bdce/main/busybox/ssl_client.c"
                "l6ULeIMMPl9KsPmufaCZH0W94xBheoS5k1u5a-56zUU="
                "/src/ssl_client.c"
                #o644)
      tools:  (cc-for-target conf)
      inputs: (list libressl musl libssp-nonshared)
      build:  (make-recipe
                script: `((cd /src)
                          (if ((,@($CC conf) ,@($CFLAGS conf) ssl_client.c -ltls -lssl -lcrypto -o ssl_client)))
                          (install -D -m "755" ssl_client /out/usr/bin/ssl_client))))))

