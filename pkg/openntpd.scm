(import
 scheme
 (distill package)
 (distill base))

(define openntpd
  (cmmi-package
   "openntpd" "6.8p1"
   "https://cdn.openbsd.org/pub/OpenBSD/OpenNTPD/$name-$version.tar.gz"
   "Zp7t2oL1zBpj8vEx-ZCbaCdGDJ0a-5pGN13iSEnhTxg="
   libs: (list libressl)
   ;; gcc warns that these two buffers are possibly too small
   ;; for the s(n)printfs that are used to write to them:
   prepare: '(if
              (sed |-i| -e "137s/11/19/" src/util.c)
              sed |-i| -e "839s/3/4/" src/ntpd.c)
   extra-configure: '(--localstatedir=/var
                      --with-cacert=/etc/ssl/cert.pem
                      --with-privsep-user=ntpd
                      ;; we'd have to mount /var anyway
                      ;; since we need rw storage for the driftfile
                      --with-privsep-path=/var/empty)
   cleanup: '(rm -rf /out/etc /out/usr/share /out/var)))
