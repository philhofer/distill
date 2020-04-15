(import
  scheme
  (only (chicken string) conc)
  (distill plan)
  (distill kvector)
  (distill package)
  (distill base))

(define dhcpcd
  (let ((src (source-template
               "dhcpcd" "9.0.0"
               "https://roy.marples.name/downloads/$name/$name-$version.tar.xz"
               "xbigG7WN0bju7nhpU3moMoiHlZRzu5PZAFOwZr2Xf6s=")))
    (lambda (conf)
      (source->package
        conf
        src
        tools:  (cc-for-target conf)
        inputs: (list linux-headers musl libssp-nonshared)
        build:  (gnu-recipe
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
