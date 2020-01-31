(import
  scheme
  (distill plan)
  (distill base)
  (distill package)
  (only (chicken string) conc))

;; ncurses uses BUILD_XXX instead of XXX_FOR_BUILD
(define (build-env-lines)
  (let* ((trim (string-length "_FOR_BUILD"))
         (pref "BUILD_")
         (xfrm (lambda (sym)
                 (let* ((str (symbol->string sym))
                        (len (string-length str)))
                   (string->symbol
                     (string-append pref (substring str 0 (- len trim))))))))
    (map
      (lambda (p)
        (pair->string= (cons (xfrm (car p)) (cdr p))))
      cc-env/build)))

(define ncurses
  (let* ((version '6.1-20200118)
         (src     (remote-archive
                    (conc "https://invisible-mirror.net/archives/ncurses/current/ncurses-" version ".tgz")
                    "Xt6yJZFzVbsW3C9DrJFKxQjtkCLP1pRLc8fTiyHqKoI=")))
    (lambda (conf)
      (make-package
        label:  (conc "ncurses-" (conf 'arch))
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
        build:  (gnu-build
                  (conc "ncurses-" version)
                  (config-prepend conf 'configure-flags
                                  `(--without-ada --without-tests
                                                  --disable-termcap
                                                  --disable-rpath-hack
                                                  --disable-stripping
                                                  --disable-pc-files
                                                  --with-terminfo-dirs=/etc/terminfo
                                                  --without-cxx-binding
                                                  --enable-widec
                                                  ,@(build-env-lines)))
                  ;; lib*w.a are source-compatible with lib*.a
                  post-install: (let ((libs '(ncurses form panel menu)))
                                  (cons
                                    `(if ((ln -s libncurses.a /out/usr/lib/libcurses.a)))
                                    (map
                                      (lambda (lib)
                                        `(if ((ln -s ,(conc "lib" lib "w.a") ,(conc "/out/usr/lib/lib" lib ".a")))))
                                      libs))))))))

