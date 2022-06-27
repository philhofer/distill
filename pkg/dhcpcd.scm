(import
 scheme
 (distill execline)
 (distill package)
 (distill base))

(define dhcpcd
  (cmmi-package
   "dhcpcd" "9.4.1"
   "https://roy.marples.name/downloads/$name/$name-$version.tar.xz"
   "7AXkBWmTh2OntSqfkyNIOsyvJzfNSqHxv30UpMg8o68="
   libs: (list linux-headers)
   ;; not autoconf
   override-configure: `(,(el= '--build= $build-triple)
                         ,(el= '--host= $triple)
                         --prefix=/usr
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
