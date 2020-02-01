(import
  scheme
  (distill plan)
  (distill package)
  (distill buildenv)
  (distill base)
  (only (chicken string) conc)
  (pkg libedit)
  (pkg ncurses)
  (pkg libressl)
  (pkg linux-headers))

(define openssh
  (let* ((version '8.1p1)
         (src     (remote-archive
                    (conc "https://ftp.openbsd.org/pub/OpenBSD/OpenSSH/portable/openssh-" version ".tar.gz")
                    "eoRwre_VyCJpPncjaxdBeTWtne82W01Utnk7Ku9Ey3w="))
         (patch0  (remote-file
                    "https://git.alpinelinux.org/aports/plain/main/openssh/fix-verify-dns-segfault.patch"
                    "Q4tNRrMwqrWYUf4WdcNkZPOmxGBnwQfd2da-34HX9wQ="
                    "/src/patch0.patch"
                    #o644)))
    (lambda (conf)
      (make-package
        label:  (conc "openssh-" (conf 'arch))
        src:    (list src patch0)
        tools:  (cc-for-target conf)
        inputs: (list linux-headers libedit ncurses zlib libressl musl libssp-nonshared)
        build:  (gnu-build
                  (conc "openssh-" version)
                  (config-prepend conf 'configure-flags
                                  '(--with-pid-dir=/run
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
                  pre-configure: (script-apply-patches (list patch0)))))))

