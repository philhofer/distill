(import
  scheme
  (distill plan)
  (distill package)
  (distill kvector)
  (distill base)
  (distill execline)
  (only (distill linux) linux-headers)
  (only (distill image) libressl)
  (only (chicken string) conc)
  (pkg libedit)
  (pkg ncurses))

(define openssh
  (let* ((version '8.2p1)
         (src     (remote-archive
                    (conc "https://ftp.openbsd.org/pub/OpenBSD/OpenSSH/portable/openssh-" version ".tar.gz")
                    "Q1b0Y6r5EMr46lYDaN-BW_WFyVptktRHSdnW9o05z5o="))
         (patch0  (remote-file
                    "https://git.alpinelinux.org/aports/plain/main/openssh/fix-verify-dns-segfault.patch"
                    "Q4tNRrMwqrWYUf4WdcNkZPOmxGBnwQfd2da-34HX9wQ="
                    "/src/patch0.patch"
                    #o644)))
    (lambda (conf)
      (make-package
        label:  (conc "openssh-" ($arch conf))
        src:    (list src patch0)
        tools:  (cc-for-target conf)
        inputs: (list linux-headers libedit ncurses zlib libressl musl libssp-nonshared)
        build:  (gnu-recipe
                  (conc "openssh-" version)
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
                    pre-configure: (+= (script-apply-patches (list patch0)))
                    post-install:  (+= (execline*
                                         ;; do NOT keep config files;
                                         ;; those are inserted via overlay
                                         (if ((rm -rf /out/var)))
                                         (if ((rm -rf /out/etc)))))))))))

