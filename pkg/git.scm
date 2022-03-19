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
   "git" "2.35.1"
   "https://www.kernel.org/pub/software/scm/$name/$name-$version.tar.gz"
   "vOj-iTMbdmiI-6Oh97bQBGfcJsJHocT0jjANz8TIlXI="
   tools: (list tar)
   libs:  (list pcre2 zlib libexpat libressl)
   ;; necessary for cross-compilation:
   env:   '((ac_cv_fread_reads_directories . yes)
            (ac_cv_snprintf_returns_bogus . no))
   override-make: `(,$make-overrides NO_GETTEXT=1)
   override-install: '(NO_GETTEXT=1 DESTDIR=/out install)
   extra-configure: '(--with-curl
                      --with-openssl --with-expat
                      --with-libpcre2 --without-tcltk
                      --without-iconv
                      --with-perl=/usr/bin/perl
                      --with-python=/usr/bin/python3)))
