(import
  scheme
  (distill execline)
  (distill package)
  (distill base))

(define dhcpcd
  (cmmi-package
   "dhcpcd" "9.3.0"
   "https://roy.marples.name/downloads/$name/$name-$version.tar.xz"
   "oxm27HuwQjFsfVBYCfY-hXLTH_0EpWVKfp2401ocZw4="
   libs: (list linux-headers)
   ;; not autoconf
   override-configure: `(,(el= '--build= $build-triple)
			 ,(el= '--host= $triple)
			 --prefix=/usr
			 --enable-static
			 --disable-debug
			 --enable-ipv4
			 --enable-ipv6
			 --enable-dhcp6
			 --enable-auth
			 --enable-privsep
			 --privsepuser=dhcpcd
			 --without-udev)
   ;; we do not use the built-in hooks; just give us /usr/sbin/dhcpcd
   cleanup: '(rm -rf /out/usr/etc /out/usr/libexec /out/usr/share)))
