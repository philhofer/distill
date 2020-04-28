(import
  scheme
  (distill package)
  (distill base))

(define opennptd
  (cmmi-package
   "openntpd" "6.2p3"
   "https://cdn.openbsd.org/pub/OpenBSD/OpenNTPD/$name-$version.tar.gz"
   "gxEKgw_Uz8JpD8jmwQEp3HrQB5VB8Ax9LT1rmAqANlM="
   libs: (list libressl)
   extra-configure: '(--with-cacert=/etc/ssl/cert.pem
		      --with-privsep-user=ntpd
		      ;; we'd have to mount /var anyway
		      ;; since we need rw storage for the driftfile
		      --with-privsep-path=/var/empty)
   cleanup: '((if ((rm -rf /out/etc /out/usr/share /out/usr/var))))))
