(import
  scheme
  (only (chicken string) conc)
  (distill plan)
  (distill kvector)
  (distill package)
  (distill base))

(define dhcpcd
  (let* ((ver '9.0.0)
         (src (remote-archive
                (conc "https://roy.marples.name/downloads/dhcpcd/dhcpcd-" ver ".tar.xz")
                "xbigG7WN0bju7nhpU3moMoiHlZRzu5PZAFOwZr2Xf6s=")))
    (lambda (conf)
      (make-package
        label:  (conc "dhcpcd-" ver "-" ($arch conf))
        src:    src
        tools:  (cc-for-target conf)
        inputs: (list linux-headers musl libssp-nonshared)
        build:  (gnu-recipe
                  (conc "dhcpcd-" ver)
                  (kwith
                    ($gnu-build conf)
                    ;; do not install configuration;
                    ;; should come from service definition
                    post-install: (+= '((if ((rm -rf /out/usr/etc)))))
                    ;; not autoconf
                    configure-args: (:= `(,(conc "--build=" build-triple)
                                           ,(conc "--host=" ($triple conf))
                                           --prefix=/usr
                                           --enable-static
                                           --disable-debug
                                           --enable-ipv4
                                           --enable-ipv6
                                           --enable-dhcp6
                                           --enable-auth
                                           --enable-privsep
                                           --privsepuser=dhcpcd
                                           --without-udev
                                           --prefix=/usr))))))))
