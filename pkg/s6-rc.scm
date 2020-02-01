(import
  scheme
  (only (chicken string)
        conc)
  (only (distill package)
        make-package)
  (only (distill plan)
        remote-archive)
  (only (distill buildenv)
        ska-build
        sysroot)
  (only (distill base)
        cc-for-target
        skalibs
        execline-tools
        musl
        libssp-nonshared)
  (pkg s6))

(define s6-rc
  (let* ((version '0.5.1.1)
         (src     (remote-archive
                    (conc "https://skarnet.org/software/s6-rc/s6-rc-" version ".tar.gz")
                    "KCvqdFSKEUNPkHQuCBfs86zE9JvLrNHU2ZhEzLYY5RY=")))
    (lambda (conf)
      (make-package
        label:  (conc "s6-rc-" (conf 'arch))
        src:    src
        tools:  (cc-for-target conf)
        inputs: (list s6 skalibs execline-tools musl libssp-nonshared)
        build:  (ska-build (conc "s6-rc-" version) conf
                           extra-configure: `(,(conc '--with-sysdeps= (sysroot conf) "/lib/skalibs/sysdeps")
                                               --enable-static-libc))))))
