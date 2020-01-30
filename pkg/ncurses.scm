(import
  scheme
  (plan)
  (base)
  (package)
  (only (chicken string) conc))

(define ncurses
  (let* ((version '6.1-20200118)
         (src     (remote-archive
                    (conc "https://invisible-mirror.net/archives/ncurses/current/ncurses-" version ".tgz")
                    "Xt6yJZFzVbsW3C9DrJFKxQjtkCLP1pRLc8fTiyHqKoI=")))
    (lambda (conf)
      (make-package
        label:  (conc "ncurses-" (conf 'arch))
        src:    src
        tools:  (cc-for-target conf)
        inputs: (list musl libssp-nonshared)
        build:  (gnu-build
                  (conc "ncurses-" version)
                  (config-prepend conf 'configure-flags
                                  '(--without-ada --without-tests
                                                  --disable-termcap
                                                  --disable-rpath-hack
                                                  --disable-stripping
                                                  --disable-pc-files
                                                  --with-terminfo-dirs=/etc/terminfo
                                                  --without-cxx-binding
                                                  --enable-widec))
                  ;; lib*w.a are source-compatible with lib*.a
                  post-install: (let ((libs '(ncurses form panel menu)))
                                  (map
                                    (lambda (lib)
                                      `(if ((ln -s ,(conc "lib" lib "w.a") ,(conc "/out/usr/lib/lib" lib ".a")))))
                                    libs)))))))

