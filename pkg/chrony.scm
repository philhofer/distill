(import
 scheme
 (only (distill base) linux-headers)
 (distill package)
 (distill execline)
 (pkg libseccomp)
 (pkg libcap)
 (pkg libedit)
 (pkg ncurses))

(define chrony
  (cmmi-package
   "chrony" "4.3"
   "https://download.tuxfamily.org/$name/$name-$version.tar.gz"
   "8u0oACiIBN4h3WcxA2OxWo7azfwVoZw6t7hZdmKQHo8="
   libs: (list libcap libseccomp linux-headers)
   tools: '()
   ;; not standard autotools
   override-configure: `(--prefix=/usr
                         --sysconfdir=/etc
                         --with-user=chrony
                         --disable-sechash
                         --enable-scfilter
                         ,(elconc "--host-machine=" $arch))))
