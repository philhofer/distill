(import
  scheme
  (distill base)
  (distill package)
  (only (chicken keyword) keyword->string string->keyword)
  (only (chicken string) conc))

(define ncurses
  (let* ((cc-for-build  (lambda ()
			  (cc-env/build
			   (lambda (kw)
			     (string->keyword
			      (string-append "BUILD_" (keyword->string kw)))))))
	 ($cc-for-build (lambda (conf)
			  (cc-for-build))))
    (cmmi-package
     "ncurses" "6.2"
     "https://invisible-mirror.net/archives/$name/$name-$version.tar.gz"
     "yMw83zJIGrC7h2uhSt_NgbUA2Rx8nn8gX6Wncy1NEek="
     ;; the ncurses installation process needs a runnable
     ;; ncurses binary, and we won't get that when cross-compiling
     tools: (lambda (conf)
	      (if (eq? ($triple conf) (build-triple))
		  '()
		  (list ncurses)))
     native-cc: cc-for-build
     cleanup: (let ((libs '(ncurses form panel menu)))
		(cons
		 `(if ((ln -s libncurses.a /out/usr/lib/libcurses.a)))
		 (map
		  (lambda (lib)
		    `(if ((ln -s ,(conc "lib" lib "w.a") ,(conc "/out/usr/lib/lib" lib ".a")))))
		  libs)))
     extra-configure: (vargs
		       `(--without-ada
			 --without-tests --disable-termcap
			 --disable-rpath-hack --disable-stripping
			 --disable-pc-files --with-terminfo-dirs=/etc/terminfo
			 --without-cxx-binding --enable-widec
			 ,$cc-for-build)))))


