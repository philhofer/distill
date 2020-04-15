(import
  scheme
  (distill plan)
  (distill package)
  (distill kvector)
  (distill base)
  (pkg libedit)
  (pkg ncurses))

(define openssh
  (let ((src (source-template
               "openssh" "8.2p1"
               "https://ftp.openbsd.org/pub/OpenBSD/OpenSSH/portable/$name-$version.tar.gz"
               "Q1b0Y6r5EMr46lYDaN-BW_WFyVptktRHSdnW9o05z5o="
               (list
                 ; patches:
                 (remote-file
                   "https://git.alpinelinux.org/aports/plain/main/openssh/fix-verify-dns-segfault.patch"
                   "Q4tNRrMwqrWYUf4WdcNkZPOmxGBnwQfd2da-34HX9wQ="
                   "/src/patch0.patch"
                   #o644)))))
    (lambda (conf)
      (source->package
        conf
        src
        tools:  (cc-for-target conf)
        inputs: (list linux-headers libedit ncurses zlib libressl musl libssp-nonshared)
        build:  (gnu-recipe
                  (kwith
                    ($gnu-build conf)
                    configure-args: (+= '(--with-pid-dir=/run
                                           --disable-lastlog
                                           --disable-strip
                                           --disable-wtmp
                                           --disable-pkcs11 ;; requires dlopen()
                                           --disable-sk
                                           --with-pie=no ;; don't pass -pie; use CFLAGS!
                                           --with-sandbox=seccomp_filter ;; fail configure if we can't use seccomp
                                           --with-privsep-path=/var/empty
                                           --with-xauth=/usr/bin/xauth
                                           --with-privsep-user=sshd
                                           --with-md5-passwords
                                           --with-libedit))
                    post-install:  (+= `(;; do NOT keep config files;
                                         ;; those are inserted via overlay
                                         (if ((rm -rf /out/var)))
                                         (if ((rm -rf /out/etc)))))))))))

