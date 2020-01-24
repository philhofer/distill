(module (pkg s6)
  (s6)
  (import
    scheme
    (only (chicken string)
          conc)
    (only (package)
          make-package)
    (only (plan)
          remote-archive)
    (only (base)
          sysroot
          cc-for-target
          skalibs
          execline-tools
          musl
          libssp-nonshared
          ska-build))

  (define s6
    (let* ((version '2.9.0.1)
           (src     (remote-archive
                      (conc "https://skarnet.org/software/s6/s6-" version ".tar.gz")
                      "uwnwdcxfc7i3LTjeNPcnouhShXzpMPIG0I2AbQfjL_I=")))
      (lambda (conf)
        (make-package
          #:label  (conc "s6-" (conf 'arch))
          #:src    src
          #:tools  (cc-for-target conf)
          #:inputs (list skalibs execline-tools musl libssp-nonshared)
          #:build  (ska-build (conc "s6-" version) conf
                              #:extra-configure `(,(conc '--with-sysdeps= (sysroot conf) "/lib/skalibs/sysdeps")
                                                   --enable-static-libc))))))

  )
