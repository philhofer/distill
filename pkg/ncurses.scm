(import
  scheme
  (distill plan)
  (distill base)
  (distill package)
  (distill kvector)
  (only (chicken keyword) keyword->string string->keyword)
  (only (chicken string) conc))

(define ncurses
  (let* ((src (source-template
                "ncurses" "6.2"
                "https://invisible-mirror.net/archives/$name/$name-$version.tar.gz"
                "yMw83zJIGrC7h2uhSt_NgbUA2Rx8nn8gX6Wncy1NEek="))
         (cc-build (cc-env/build (lambda (kw)
                                   (string->keyword
                                     (string-append "BUILD_" (keyword->string kw)))))))
    (lambda (conf)
      (source->package
        conf
        src
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

