(import
  scheme
  (distill plan)
  (distill base)
  (distill package)
  (distill kvector)
  (only (chicken keyword) keyword->string string->keyword)
  (only (chicken string) conc))

(define ncurses
  (let* ((version  '6.1-20200118)
         (src      (remote-archive
                     (conc "https://invisible-mirror.net/archives/ncurses/current/ncurses-" version ".tgz")
                     "Xt6yJZFzVbsW3C9DrJFKxQjtkCLP1pRLc8fTiyHqKoI="))
         (cc-build (cc-env/build (lambda (kw)
                                   (string->keyword
                                     (string-append "BUILD_" (keyword->string kw)))))))
    (lambda (conf)
      (make-package
        label:  (conc "ncurses-" ($arch conf))
        src:    src
        tools:  (+cross
                  conf
                  (append
                    (cc-for-target conf)
                    (native-toolchain-for conf))
                  ;; yes, ncurses depends on a native version of itself
                  ;; for cross-builds... need tic(1)
                  (list ncurses))
        inputs: (list musl libssp-nonshared)
        build:  (gnu-recipe
                  (conc "ncurses-" version)
                  (kwith
                    ($gnu-build conf)
                    configure-args: (+= `(--without-ada --without-tests
                                                        --disable-termcap
                                                        --disable-rpath-hack
                                                        --disable-stripping
                                                        --disable-pc-files
                                                        --with-terminfo-dirs=/etc/terminfo
                                                        --without-cxx-binding
                                                        --enable-widec
                                                        ,@(kvargs cc-build)))
                    ;; lib*w.a are source-compatible with lib*.a
                    post-install: (+= (let ((libs '(ncurses form panel menu)))
                                        (cons
                                          `(if ((ln -s libncurses.a /out/usr/lib/libcurses.a)))
                                          (map
                                            (lambda (lib)
                                              `(if ((ln -s ,(conc "lib" lib "w.a") ,(conc "/out/usr/lib/lib" lib ".a")))))
                                            libs))))))))))

