(import
  scheme
  (scheme base)
  (base)
  (plan)
  (package)
  (execline)
  (only (chicken string) conc)
  (pkg xz-utils))

(define linux-headers
  (let* ((version  '5.4)
         (patchlvl 15)
         (src     (list
                    (remote-archive
                      (conc "https://cdn.kernel.org/pub/linux/kernel/v5.x/linux-" version ".tar.xz")
                      "SUt0rAz8S3yXkXuSN8sG6lm4sW7Bvssxg_oAKuNjqzs=")
                    (remote-file
                      (conc "https://cdn.kernel.org/pub/linux/kernel/v5.x/patch-" version "." patchlvl ".xz")
                      "DclHfO37RNB0C0ffq15VHvPY1a88IdSPP-3gZ1HLL6w="
                      "/src/linux.patch"
                      #o644)))
         (arch-name (lambda (arch)
                      (case arch
                        ((x86_64)        'x86_64)
                        ((x86)           'i386)
                        ((ppc64 ppc64le) 'powerpc)
                        ((aarch64)       'arm64)
                        ((armv7)         'arm)
                        (else (error "unrecognized arch" arch))))))
    (lambda (conf)
      (make-package
        parallel: #f
        src:    src
        label:  (conc "linux-headers-" (conf 'arch))
        tools:  (append (list execline-tools busybox-core make xz-utils)
                        (native-toolchain))
        inputs: '()
        build:  (make-recipe
                  script: (execline*
                            (cd ,(conc "linux-" version))
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
