
(define *major* 5.4)
(define *patch* 19)

(define *linux-source*
  (list
    (remote-archive
      (conc "https://cdn.kernel.org/pub/linux/kernel/v5.x/linux-" *major* ".tar.xz")
      "SUt0rAz8S3yXkXuSN8sG6lm4sW7Bvssxg_oAKuNjqzs=")
    (remote-file
      (conc "https://cdn.kernel.org/pub/linux/kernel/v5.x/patch-" *major* "." *patch* ".xz")

      "FU3Y09l8z9uV-SyBgt2mNco0ZeMdWl-adEvRXaWgVko="
      "/src/linux.patch"
      #o644)))

(define (arch-name arch)
  (case arch
    ((x86_64)        'x86_64)
    ((x86)           'i386)
    ((ppc64 ppc64le) 'powerpc)
    ((aarch64)       'arm64)
    ((armv7)         'arm)
    (else (error "unrecognized arch" arch))))

(define linux-headers
  (let ()
    (lambda (conf)
      (make-package
        parallel: #f
        src:    *linux-source*
        label:  (conc "linux-headers-" *major* "." *patch* "-" (conf 'arch))
        tools:  (append (list execline-tools busybox-core make xz-utils)
                        (native-toolchain))
        inputs: '()
        build:  (make-recipe
                  script: (execline*
                            (cd ,(conc "linux-" *major*))
                            (if ((pipeline ((xzcat /src/linux.patch)))
                                 (patch -p1)))
                            (if ((make ,(conc 'ARCH= (arch-name (conf 'arch)))
                                  ,@(map pair->string= (cc-env/kbuild))
                                  headers)))
                            ;; headers_install uses rsync, which is a
                            ;; silly large dependency to pull in
                            ;; at this stage...
                            (if ((cp -r usr /out)))
                            (find /out -type f ! -name "*.h" -delete)))))))
