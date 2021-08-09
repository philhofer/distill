(import
 scheme
 (distill base)
 (distill package)
 (only (chicken module) export)

 (pkg pkgconf))

;; NOTE: many of the binaries produced by util-linux
;; will conflict with a busybox install, so you should
;; probably use subpackages of util-linux instead
(define util-linux
  (cmmi-package
   "util-linux" "2.37.1"
   "https://mirrors.edge.kernel.org/pub/linux/utils/$name/v2.37/$name-$version.tar.gz"
   "e9bdJJ8apQ4R0IUW_r2lj4UnmgEWPBptOC1muXJg288="
   tools: (list pkgconf)
   libs: (list linux-headers)
   extra-configure: '(--without-python
                      --without-udev
                      --disable-nologin
                      --disable-sulogin
                      --disable-makeinstall-chown
                      --disable-makeinstall-setuid)))

(export sfdisk)
(define sfdisk
  (subpackage "sfdisk-" util-linux "./sbin/sfdisk"))
