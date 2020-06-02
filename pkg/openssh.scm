(import
  scheme
  (distill plan)
  (distill package)
  (distill base)
  (pkg libedit)
  (pkg ncurses))

(define openssh
  (cmmi-package
   "openssh" "8.3p1"
   "https://ftp.openbsd.org/pub/OpenBSD/OpenSSH/portable/$name-$version.tar.gz"
   "6V6bSpzwnMOaF-0R6fQRFmnKXlKFdjjancT_phfoq7M="
   patches: (patchfiles* "patches/openssh/fix-dns.patch")
   libs: (list linux-headers libedit ncurses zlib libressl)
   extra-configure: '(--with-pid-dir=/run
		      --disable-lastlog
		      --disable-strip
		      --disable-wtmp
		      --disable-pkcs11 ; requires dlopen() ;
		      --disable-sk
		      --with-pie=no ; don't pass -pie; use CFLAGS! ;
		      --with-sandbox=seccomp_filter ; fail configure if we can't use seccomp
		      --with-privsep-path=/var/empty
		      --with-xauth=/usr/bin/xauth
		      --with-privsep-user=sshd
		      --with-md5-passwords
		      --with-libedit)
   cleanup: '(;; do NOT keep config files;
	      ;; those are inserted via overlay
	      (if ((rm -rf /out/var)))
	      (if ((rm -rf /out/etc))))))
