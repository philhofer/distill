(import
 scheme
 (distill execline)
 (distill package)
 (distill base))

(define dhcpcd
  (cmmi-package
   "dhcpcd" "10.0.0"
   "https://github.com/NetworkConfiguration/$name/archive/refs/tags/$name-$version.tar.gz"
   "Vux9v66pKUtYJxUhAkGz-Shk2rvdook1Vjg1lJCQL3Q="
   dir: "dhcpcd-dhcpcd-10.0.0"
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
