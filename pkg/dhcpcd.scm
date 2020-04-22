(import
  scheme
  (distill package)
  (distill base))

(define dhcpcd
  (cmmi-package
   "dhcpcd" "9.0.0"
   "https://roy.marples.name/downloads/$name/$name-$version.tar.xz"
   "xbigG7WN0bju7nhpU3moMoiHlZRzu5PZAFOwZr2Xf6s="
   libs: (list linux-headers)
   ;; not autoconf
   override-configure: (vargs `((--build= ,$build-triple)
				(--host= ,$triple)
				--prefix=/usr
				--enable-static
				--disable-debug
				--enable-ipv4
				--enable-ipv6
				--enable-dhcp6
				--enable-auth
				--enable-privsep
				--privsepuser=dhcpcd
				--without-udev))
   cleanup: '((if ((rm -rf /out/usr/etc))))))
