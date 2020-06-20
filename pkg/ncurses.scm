(import
  scheme
  (distill base)
  (distill package)
  (distill execline)
  (only (chicken keyword) keyword->string string->keyword)
  (only (chicken string) conc))

(define ncurses
  (let (($cc-for-build  (lambda (conf)
			  (cc-env/build
			   conf
			   (lambda (kw)
			     (string->keyword
			      (string-append "BUILD_" (keyword->string kw))))))))
    (cmmi-package
     "ncurses" "6.2"
     "https://invisible-mirror.net/archives/$name/$name-$version.tar.gz"
     "yMw83zJIGrC7h2uhSt_NgbUA2Rx8nn8gX6Wncy1NEek="
     ;; the ncurses installation process needs a runnable
     ;; ncurses binary, and we won't get that when cross-compiling
     cross: (list (lambda (conf)
		    (if (eq? ($triple conf) ($build-triple conf))
			'()
			(list ncurses))))
     native-cc: $cc-for-build
     cleanup: (elif*
	       '(ln -s libncurses.a  /out/usr/lib/libcurses.a)
	       '(ln -s libncursesw.a /out/usr/lib/libncurses.a)
	       '(ln -s libformw.a    /out/usr/lib/libform.a)
	       '(ln -s libpanelw.a   /out/usr/lib/libpanel.a)
	       '(ln -s libmenuw.a    /out/usr/lib/libmenu.a))
     extra-configure: `(--without-ada
			--without-tests --disable-termcap
			--disable-rpath-hack --disable-stripping
			--disable-pc-files --with-terminfo-dirs=/etc/terminfo
			--without-cxx-binding --enable-widec
			,$cc-for-build))))


