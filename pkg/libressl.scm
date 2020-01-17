;; busybox needs libcrypto and ssl-client needs libtls
(define libressl
  (let* ((version '3.0.2)
	 (leaf    (remote-archive
		    (conc "https://ftp.openbsd.org/pub/OpenBSD/LibreSSL/libressl-" version ".tar.gz")
		    "klypcg5zlwvSTOzTQZ7M-tBZgcb3tPe72gtWn6iMTR8=")))
    (package-lambda
      conf
      (make-package
	#:label  (conc "libressl-" version "-" (conf 'arch))
	#:src    leaf
	#:tools  (cc-for-target conf)
	#:inputs libc
	#:build  (gnu-build
		   (conc "libressl-" version)
		   conf
		   #:post-install (execline*
				    (if ((ln -s openssl /out/usr/bin/libressl)))))))))

;; TLS client for wget (shamelessly lifted from Alpine)
(define ssl-client
  (let ()
    (package-lambda
      conf
      (make-package
	#:label  (conc "ssl-client-" (conf 'arch))
	#:src    (fetch-remote-file!
		   "https://raw.githubusercontent.com/alpinelinux/aports/bb3bc00f304cb4f0611d45555d124221d365bdce/main/busybox/ssl_client.c"
		   "l6ULeIMMPl9KsPmufaCZH0W94xBheoS5k1u5a-56zUU="
		   "/src/ssl_client.c"
		   #o644)
	#:tools  (cc-for-target conf)
	#:inputs (cons libressl libc)
	#:build  (make-recipe
		   #:script (let* ((cenv   (cc-env conf))
				   (CC     (cdr (assq 'CC cenv)))
				   (CFLAGS (cdr (assq 'CFLAGS cenv))))
			      (execline*
				(cd /src)
				(if ((,CC ,@CFLAGS ssl_client.c -ltls -lssl -lcrypto -o ssl_client)))
				(install -D -m "755" ssl_client /out/usr/bin/ssl_client))))))))
