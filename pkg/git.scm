(import
  scheme
  (only (chicken string) conc)
  (distill plan)
  (distill kvector)
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
  (let* ((ver '2.26.0)
         (src (remote-archive
                (conc "https://www.kernel.org/pub/software/scm/git/git-" ver ".tar.gz")
                "43UgMe5coBw_rTJvgpz66JsoZ5ZWkTUQBQcLnmAGi3w=")))
    (lambda (conf)
      (make-package
        label:  (conc "git-" ver "-" ($arch conf))
        src:    src
        tools:  (cons tar (cc-for-target conf))
        inputs: (list pcre2 zlib libexpat libressl musl libssp-nonshared)
        build:  (gnu-recipe
                  (conc "git-" ver)
                  (kwith
                    ($gnu-build conf)
                    ;; these need to be exported for cross-compilation;
                    ;; otherwise the configure script will complain
                    pre-configure: (+= '((export ac_cv_fread_reads_directories yes)
                                         (export ac_cv_snprintf_returns_bogus no)))
                    make-flags: (+= '(NO_GETTEXT=1))
                    install-flags: (:= '(NO_GETTEXT=1 DESTDIR=/out install))
                    configure-args: (+= '(--with-curl
                                           --with-openssl
                                           --with-expat
                                           --with-libpcre2
                                           --without-tcltk
                                           --without-iconv
                                           --with-perl=/usr/bin/perl
                                           --with-python=/usr/bin/python3))))))))
