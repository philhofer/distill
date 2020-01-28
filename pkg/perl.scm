(import
  scheme
  (only (chicken base) unless)
  (only (chicken string) conc)
  (filepath)
  (execline)
  (eprint)
  (plan)
  (package)
  (base))

;; perl packages a statically-linked perl binary
;;
;; NOTE: cross-compiling perl using the standard 'Configure' script
;; requires ssh access to the target machine (in order to run code!),
;; which is very obviously not something we can support, so this
;; code would have to learn *a lot* about perl build configuration internals
;; in order to make cross-compilation work
(define perl
  (let* ((version '5.30.1)
         (leaf    (remote-archive
                    (conc "https://www.cpan.org/src/5.0/perl-" version ".tar.gz")
                    "EBvfwKXjX_aaet0AXxwJKJGpGy4RnUrrYjh-qLeZ8ts=")))
    (lambda (conf)
      (unless (eq? (conf 'arch) *this-machine*)
        (fatal "one does not simply cross-compile perl; please read the comment(s) in perl.scm"))
      (make-package
        label:  (conc "perl-" version "-" (conf 'arch))
        src:    leaf
        tools:  (cc-for-target conf)
        inputs: (list zlib bzip2 musl libssp-nonshared)
        build:
        (let* ((cenv            (cc-env conf))
               (menv            (make-env conf))
               (toolpre         (conc (triple conf) "-"))
               (CC              (assq 'CC cenv))
               (LD              (assq 'LD cenv))
               (CFLAGS          (assq 'CFLAGS cenv))
               (LDFLAGS         (assq 'LDFLAGS cenv))
               (NM              (assq 'NM menv))
               (RANLIB          (assq 'RANLIB menv))
               (AR              (assq 'AR menv))
               (ARFLAGS         (assq 'ARFLAGS menv))
               (-Dflags         (map pair->string=
                                     `((-Dccflags .  ,(cdr CFLAGS))
                                       (-Darflags .  ,(cdr ARFLAGS))
                                       (-Dldflags .  ,(cdr LDFLAGS))
                                       (-Dcc .       ,(cdr CC))
                                       (-Dld .       ,(cdr LD))
                                       (-Doptimize . ,(or (conf 'CFLAGS) '-Os))
                                       (-Dnm       . ,(conc (triple conf) "-nm"))
                                       (-Dranlib   . ,(cdr RANLIB))
                                       (-Dar       . ,(cdr AR))
                                       (-Dsysroot  . ,(sysroot conf)))))
               ;; yes, this is gross
               ;; but not as gross as perl's 'Configure' script
               (configure-flags `(,@-Dflags
                                   -Dprefix=/usr -Dprivlib=/usr/share/perl5/core_perl
                                   -Darchlib=/usr/lib/perl5/core_perl -Dvendorprefix=/usr
                                   -Dvendorlib=/usr/share/perl5/vendor_perl -Dvendorarch=/usr/lib/perl5/vendor_perl
                                   -Duselargefiles -Dusethreads -Duseshrplib=false -Dd_semctl_semun
                                   -Dman1dir=/usr/share/man/man1 -Dman3dir=/usr/share/man/man3
                                   -Dinstallman1dir=/usr/share/man/man1 -Dinstallman3dir=/usr/share/man/man3
                                   -Dman1ext=1 -Dman3ext=3pm -Dcf_by=sysplan -Ud_csh -Uusedl -Dusenm
                                   -Dusemallocwrap)))
          (make-recipe
            env:   `((BUILD_ZLIB . 0)
                     (BUILD_BZIP2 . 0)
                     (BZIP2_LIB . ,(filepath-join (sysroot conf) "/usr/lib"))
                     (BZIP2_INCLUDE . ,(filepath-join (sysroot conf) "/usr/include")))
            script: (execline*
                      (cd ,(conc "perl-" version))
                      (if ((./Configure -des ,@configure-flags)))
                      (if ((importas -u nproc nproc)
                           (make -j $nproc ,@(makeflags conf))))
                      (if ((make DESTDIR=/out install)))
                      (if ((rm -rf /out/usr/share/man)))
                      (find /out -name ".*" -delete))))))))

