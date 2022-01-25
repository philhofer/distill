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
   "chrony" "4.2"
   "https://download.tuxfamily.org/$name/$name-$version.tar.gz"
   "tTvRRiiUAfcfX-XAozzwptSE9kzkrPY_CG73EUn_zY8="
   libs: (list libcap libseccomp linux-headers)
   tools: '()
   ;; not standard autotools
   override-configure: `(--prefix=/usr
                         --sysconfdir=/etc
                         --with-user=chrony
                         --disable-sechash
                         --enable-scfilter
                         ,(elconc "--host-machine=" $arch))))
