(import
  scheme
  (only (chicken string) conc)
  (execline)
  (plan)
  (base)
  (package))

;; busybox needs libcrypto and ssl-client needs libtls
(define libressl
  (let* ((version '3.0.2)
         (leaf    (remote-archive
                    (conc "https://ftp.openbsd.org/pub/OpenBSD/LibreSSL/libressl-" version ".tar.gz")
                    "klypcg5zlwvSTOzTQZ7M-tBZgcb3tPe72gtWn6iMTR8=")))
    (lambda (conf)
      (make-package
        #:label  (conc "libressl-" version "-" (conf 'arch))
        #:src    leaf
        #:tools  (cc-for-target conf)
        #:inputs (list musl libssp-nonshared)
        #:build  (gnu-build
                   (conc "libressl-" version)
                   conf
                   #:post-install (execline*
                                    (if ((ln -s openssl /out/usr/bin/libressl)))))))))
