(import
  scheme
  (distill package)
  (distill base)
  (pkg tar)
  (pkg pcre2)
  (pkg libexpat)
  (pkg python3))

;; NOTE: git has run-time dependencies on perl and python;
;; we don't have a way of expressing that yet...
;; also, we can't cross-compile perl, so some features
;; of git may not be available for a cross-compiled target...
(define git
  (cmmi-package
   "git" "2.26.0"
   "https://www.kernel.org/pub/software/scm/$name/$name-$version.tar.gz"
   "43UgMe5coBw_rTJvgpz66JsoZ5ZWkTUQBQcLnmAGi3w="
   tools: (list tar)
   libs:  (list pcre2 zlib libexpat libressl)
   ;; necessary for cross-compilation:
   env:   '((ac_cv_fread_reads_directories . yes)
	    (ac_cv_snprintf_returns_bogus . no))
   override-make: (vargs `(,$make-overrides NO_GETTEXT=1))
   override-install: '(NO_GETTEXT=1 DESTDIR=/out install)
   extra-configure: '(--with-curl
		      --with-openssl --with-expat
		      --with-libpcre2 --without-tcltk
		      --without-iconv
		      --with-perl=/usr/bin/perl
		      --with-python=/usr/bin/python3)))
