(define perl
  (let* ((version '5.30.1)
         (leaf    (remote-archive
                    (conc "https://www.cpan.org/src/5.0/perl-" version ".tar.gz")
                    "EBvfwKXjX_aaet0AXxwJKJGpGy4RnUrrYjh-qLeZ8ts=")))
    (package-lambda
      conf
      (unless (eq? (conf 'arch) *this-machine*)
        (error "don't know how to cross-compile perl yet :("))
      (make-package
        #:label  (conc "perl-" version "-" (conf 'arch))
        #:src    leaf
        #:tools  (cc-for-target conf)
        #:inputs (list zlib bzip2 musl #;libssp-nonshared)
                       #:build
                       (let* ((cenv            (cc-env conf))
                              (toolpre         (conc (triple conf) "-"))
                              (CC              (cdr (assq 'CC cenv)))
                              (LD              (cdr (assq 'LD cenv)))
                              (AR              (cdr (assq 'AR cenv)))
                              ;; yes, this is gross
                              ;; but not as gross as perl's 'Configure' script
                              (configure-flags `(,(conc "\"-Dccflags=" (sysroot-flag conf) " -fPIE -static-pie\"")
                                                  ,(conc "\"-Dldflags=" (sysroot-flag conf) " -static-pie\"")
                                                  ,(pair->quoted-string (cons '-Doptimize (or (conf 'CFLAGS) '-Os)))
                                                  ,(conc "-Dcc=" CC) ,(conc "-Dld=" LD) ,(conc "-Dar=" AR)
                                                  ,(conc "-Dnm=" toolpre "nm") ,(conc "-Dranlib=" toolpre "ranlib")
                                                  ,(conc "-Dsysroot=" (sysroot conf))
                                                  -Dprefix=/usr -Dprivlib=/usr/share/perl5/core_perl
                                                  -Darchlib=/usr/lib/perl5/core_perl -Dvendorprefix=/usr
                                                  -Dvendorlib=/usr/share/perl5/vendor_perl -Dvendorarch=/usr/lib/perl5/vendor_perl
                                                  -Duselargefiles -Dusethreads -Duseshrplib=false -Dd_semctl_semun
                                                  -Dman1dir=/usr/share/man/man1 -Dman3dir=/usr/share/man/man3
                                                  -Dinstallman1dir=/usr/share/man/man1 -Dinstallman3dir=/usr/share/man/man3
                                                  -Dman1ext=1 -Dman3ext=3pm -Dcf_by=sysplan -Ud_csh -Uusedl -Dusenm
                                                  -Dusemallocwrap)))
                         (make-recipe
                           #:env   `((BUILD_ZLIB . 0)
                                     (BUILD_BZIP2 . 0)
                                     (BZIP2_LIB . ,(filepath-join (sysroot conf) "/usr/lib"))
                                     (BZIP2_INCLUDE . ,(filepath-join (sysroot conf) "/usr/include")))
                           #:script (execline*
                                      (cd ,(conc "perl-" version))
                                      (if ((./Configure -des ,@configure-flags)))
                                      (if ((backtick -n -D 4 ncpu ((nproc)))
                                           (importas -u ncpu ncpu)
                                           (make -j $ncpu ,@(makeflags conf))))
                                      (if ((make DESTDIR=/out install)))
                                      (if ((rm -rf /out/usr/share/man)))
                                      (find /out -name ".*" -delete))))))))

