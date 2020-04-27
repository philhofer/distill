(import
  scheme
  (distill package)
  (distill base))

(define dhcpcd
  (cmmi-package
   "dhcpcd" "9.0.2"
   "https://roy.marples.name/downloads/$name/$name-$version.tar.xz"
   "2g-1hzbyiNaNewwW8-72GQv82GpP3aKdEWbpo7igfTQ="
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
   ;; we do not use the built-in hooks; just give us /usr/sbin/dhcpcd
   cleanup: '((if ((rm -rf /out/usr/etc /out/usr/libexec /out/usr/share))))))
