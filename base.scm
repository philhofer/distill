(define-syntax package-lambda
  (syntax-rules ()
    ((_ conf body* ...)
     (memoize-eq
       (lambda (conf) body* ...)))))

;; this is the list of packages present
;; in a bootstrap archive (in other words,
;; packages in '#:tools' that are superseded)
;;
;; note: if you're building an external bootstrap tarball,
;; it needs to provide *equivalent* packages to the list
;; below (not necessarily identical ones)
(define (bootstrap-packages conf)
  (append
    (list
      libssp-nonshared
      musl
      busybox)
    (cc-for-target conf)))

(define (triple conf)
  (string->symbol
    (conc (conf 'arch) "-none-linux-musl")))

;; pkgs->bootstrap takes a list of package-lambdas
;; and replaces dependencies from 'tools' with
;; those in the bootstrap tarball
(define (pkgs->bootstrap . pkgs)
  (let ((ht (make-hash-table)))
    (define (inner pkg)
      (or (hash-table-ref/default ht pkg #f)
	  (let ((newproc (memoize-eq
			   (lambda (conf)
			     (let ((old      (pkg conf))
				   (bootpkgs (bootstrap-packages conf)))
			       (update-package
				 old
				 #:label (conc (package-label old) "-bootstrap")
				 #:src   (flatten (bootstrap-archive *this-machine*) (package-src old))
				 #:tools (foldl
					   (lambda (lst p)
					     (if (memq p bootpkgs) lst (cons (inner p) lst)))
					   '()
					   (package-tools old))
				 ;; inputs are simply replaced with versions
				 ;; built with the bootstrap toolchain
				 #:inputs (map inner (package-inputs old))))))))
	    (hash-table-set! ht pkg newproc)
	    newproc)))
    (map inner pkgs)))

(define (config-prepend conf label lst)
  (let ((val (append lst (or (conf label) '()))))
    (lambda (sym)
      (if (eq? sym label) val (conf sym)))))

;; placeholder produces a package-lambda that errors when called
(define (placeholder name)
  (package-lambda conf
    (error (conc "package " name " not implemented yet"))))

(define gcc-for-target
  (memoize-eq
    (lambda (target) (placeholder "gcc"))))

(define busybox (placeholder "busybox"))

(define (cc-for-target conf)
  (list (gcc-for-target conf) (binutils-for-target conf) busybox))

(define bootstrap-archive
  (let ((x86-64 (local-archive
		  'tar.zst
		  "s2BsxT2v1Qe0rX-1YNSPTfEGTOz8AskqA4JClsZqgpg=")))
    (lambda (arch)
      (case arch
	((x86_64) x86-64)
	(else     (error "no bootstrap for arch" arch))))))

;; cc-env takes a configuration and produces
;; an alist with at least CFLAGS, LDFLAGS, and CPPFLAGS
;; corresponding to a list of those configuration options
;; with mandatory flags also set
(define cc-env
  (memoize-eq
    (lambda (conf)
      (let ((need-cflags    '(--sysroot=/sysroot -fPIE -static-pie))
	    (need-ldflags   '(--sysroot=/sysroot -static-pie))
	    (need-cppflags  '(--sysroot=/sysroot))
	    (cflags          (conf 'CFLAGS))
	    (ldflags         (conf 'LDFLAGS))
	    (cppflags        (conf 'CPPFLAGS))
	    (join            (lambda (a b)
			       (cond
				 ((eq? a #f) b)
				 ((list? a)  (append a b))
				 (else (cons a b))))))
	`((CFLAGS . ,(join cflags need-cflags))
	  (LDFLAGS . ,(join ldflags need-ldflags))
	  (CPPFLAGS . ,(join cppflags need-cppflags)))))))

(define (apply-conc x)
  (if (list? x)
      (if (null? x)
	  ""
	  (let ((lst (let loop ((head (car x))
				(rest (cdr x)))
		       (if (null? rest)
			   (cons head '())
			   (cons head (cons " " (loop (car rest) (cdr rest))))))))
	    (apply conc lst)))
      x))

(define (pair->quoted-string p) (conc "\"" (car p) "=" (apply-conc (cdr p)) "\""))

;; configure-args produces arguments to an autotools configure script
;; NOTE: some packages contain a script called 'configure' that isn't
;; actually generated by libtool, in which case this function may not produce
;; appropriate arguments
(define configure-args
  (memoize-eq
    (lambda (conf)
      (let* ((copts     (cc-env conf))
	     (copts-str (foldl (lambda (lst opt)
				(cons (pair->quoted-string opt) lst))
			       '()
			       copts))
	     ;; note: even though we always pass -fPIE, we need --with-pic
	     ;; to work around bugs in binutils regarding handling of TLS (sigh)
	     (need-conf `(--disable-shared --disable-nls --enable-static --enable-pie --with-pic --prefix=/usr --sysconfdir=/etc --build ,*this-machine* --host ,(conf 'arch)))
	     (usr-conf  (conf 'configure-flags)))
	(cond
	  ((eq? usr-conf #f) (append copts-str need-conf))
	  ((list? usr-conf)  (append usr-conf copts-str need-conf))
	  (else              (cons usr-conf (append copts-str need-conf))))))))

;; produce a libssp_nonshared independent of gcc to break a dependency cycle
(define libssp-nonshared
  (package-lambda
    conf
    (make-package
	#:label   (conc "libssp-nonshared-" (conf 'arch))
	#:src     '()
	#:tools   (cc-for-target conf)
	#:inputs  '()
	#:overlay (interned "/src/ssp-nonshared.c" #o644 #<<EOF
extern void __stack_chk_fail(void);
void __attribute__((visibility ("hidden"))) __stack_chk_fail_local(void) { __stack_chk_fail(); }

EOF
)
	#:build (make-recipe
		  #:script (execline*
			     (cd ./src)
			     ;; XXX don't use 'gcc' here
			     (if ((gcc -c -fPIE -Os ssp-nonshared.c -o __stack_chk_fail_local.o)))
			     (if ((ar Ur libssp_nonshared.a __stack_chk_fail_local.o)))
			     (if ((mkdir -p /out/usr/lib)))
			     (cp libssp_nonshared.a /out/usr/lib/libssp_nonshared.a))))))

(define musl
  (let* ((version '1.1.24)
	 (leaf    (remote-archive
		    (conc "https://www.musl-libc.org/releases/musl-" version ".tar.gz")
		    "hC6Gf8nyJQAZVYJ-tNG0iU0dRjES721p0x1cqBp2Ge8="))
	 (->cflags  (lambda (conf)
		      (pair->quoted-string (cons 'CFLAGS (cons '-fPIE (or (conf 'CFLAGS) '())))))))
    (package-lambda
      conf
      (make-package
	#:label (conc "musl-" version "-" (conf 'arch))
	#:src   leaf
	#:tools (cc-for-target conf)
	#:inputs '()
	#:build (make-recipe
		  #:script (execline*
			     (cd ,(conc "musl-" version))
			     ;; note: NOT autotools; --target means something different
			     (if ((./configure --disable-shared --enable-static --prefix=/usr ,(->cflags conf) --target ,(conf 'arch))))
			     (if ((backtick -n -D 4 ncpu ((nproc)))
				  (importas -u ncpu ncpu)
				  (make -j $ncpu)))
			     (make DESTDIR=/out install)))))))

;; wrapper around the 'configure;make;make install' pattern,
;; taking care to set configure flags make flags appropriately
;; for the common case that we're dealing with autotools
(define (gnu-build dir target #!key (pre-configure '()) (post-install '()) (make-flags '()) (configure #f))
  (make-recipe
    #:script (execline*
	       (cd ,dir)
	       ,@pre-configure
	       (if ((./configure ,@(or configure (configure-args target)))))
	       ;; it's helpful (mandatory?) that the script not change
	       ;; based on the number of cpus on the host, so we have
	       ;; to make that determination in the script itself
	       (if ((backtick -n -D 4 ncpu ((nproc)))
		    (importas -u ncpu ncpu)
		    (make -j $ncpu ,@make-flags)))
	       (if ((make DESTDIR=/out install)))
	       ,@post-install
	       ;; we don't care if these two succeed
	       ;; TODO: perhaps gnu packages should
	       ;; symlink makeinfo to /usr/bin/true
	       (foreground ((rm -rf /out/usr/share/man)))
	       (foreground ((rm -rf /out/usr/share/info)))
	       (true))))

(define gawk
  (let* ((version '5.0.1)
	 (leaf    (remote-archive
		    (conc "https://ftp.gnu.org/gnu/gawk/gawk-" version ".tar.xz")
		    "R3Oyp6YDumBd6v06rLYd5U5vEOpnq0Ie1MjwINSmX-4=")))
    (package-lambda
      conf
      (make-package
	#:label  (conc "gawk-" version "-" (conf 'arch))
	#:src    leaf
	#:tools  (cc-for-target conf)
	#:inputs (list musl libssp-nonshared)
	#:build  (gnu-build (conc "gawk-" version) conf)))))

(define libgmp
  (let* ((version '6.1.2)
	 (leaf    (remote-archive
		    (conc "https://gmplib.org/download/gmp/gmp-" version ".tar.xz")
		    "bodUs2nnuExA5OmrihkTLOR9P-4cmpWyeyGYiFUPM8s=")))
    (package-lambda
      conf
      (make-package
	#:label  (conc "gmp-" version "-" (conf 'arch))
	#:src    leaf
	#:tools  (cons m4 (cc-for-target conf))
	#:inputs (list musl libssp-nonshared)
	#:build  (gnu-build (conc "gmp-" version) conf)))))

(define libmpfr
  (let* ((version '4.0.2)
	 (leaf    (remote-archive
		    (conc "https://www.mpfr.org/mpfr-current/mpfr-" version ".tar.bz2")
		    "wKuAJV_JEeh560Jgqo8Iub6opUuqOKFfQATGEJ2F3ek=")))
    (package-lambda
      conf
      (make-package
	#:label  (conc "mpfr-" version "-" (conf 'arch))
	#:src    leaf
	#:tools  (cc-for-target conf)
	#:inputs (list musl libssp-nonshared libgmp)
	#:build  (gnu-build (conc "mpfr-" version) conf)))))

(define libmpc
  (let* ((version '1.1.0)
	 (leaf    (remote-archive
		    (conc "https://ftp.gnu.org/gnu/mpc/mpc-" version ".tar.gz")
		    "2lH9nuHFlFtOyT_jc5k4x2CHCtir_YwwX9mg6xoGuTc=")))
    (package-lambda
      conf
      (make-package
	#:label  (conc "mpc-" version "-" (conf 'arch))
	#:src    leaf
	#:tools  (cc-for-target conf)
	#:inputs (list musl libssp-nonshared libgmp libmpfr)
	#:build  (gnu-build (conc "mpc-" version) conf)))))

(define m4
  (let* ((version '1.4.18)
	 (leaf    (remote-archive
		    (conc "https://ftp.gnu.org/gnu/m4/m4-" version ".tar.gz")
		    "_Zto8BBAes0pngDpz96kt5-VLF6oA0wVmLGqAVBdHd0=")))
    (package-lambda
      conf
      (make-package
	#:label  (conc "m4-" version "-" (conf 'arch))
	#:src    leaf
	#:tools  (cc-for-target conf)
	#:inputs (list musl libssp-nonshared)
	#:build  (gnu-build (conc "m4-" version) conf
			    ;; m4 sticks a file in /usr/lib/charset.alias
			    #:post-install (execline*
					     (rm -rf /out/usr/lib)))))))

(define bzip2
  (let* ((version '1.0.8)
	 (leaf    (remote-archive
		    (conc "https://sourceware.org/pub/bzip2/bzip2-" version ".tar.gz")
		    "pZGXjBOF4VYQnwdDp2UYObANElrjShQaRbMDj5yef1A=")))
    (package-lambda
      conf
      (make-package
	#:label (conc "bzip2-" version "-" (conf 'arch))
	#:src   leaf
	#:tools (cc-for-target conf)
	#:inputs (list musl libssp-nonshared)
	#:build (make-recipe
		  #:script (execline*
			     (cd ,(conc "bzip2-" version))
			     (if ((make ,@(map pair->quoted-string (cc-env conf)) all)))
			     (make PREFIX=/out/usr install)))))))

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
	#:label (conc "perl-" version "-" (conf 'arch))
	#:src   leaf
	#:tools (cc-for-target conf)
	#:inputs (list zlib bzip2 musl libssp-nonshared)
	#:build
	(let ((configure-flags `("\"-Dccflags=--sysroot=/sysroot -fPIE -static-pie\"" "\"-Dldflags=--sysroot=/sysroot -static-pie\""
				 ,(pair->quoted-string (cons '-Doptimize (or (conf 'CFLAGS) '-Os)))
				 -Dsysroot=/sysroot
				 -Dprefix=/usr -Dprivlib=/usr/share/perl5/core_perl
				 -Darchlib=/usr/lib/perl5/core_perl -Dvendorprefix=/usr
				 -Dvendorlib=/usr/share/perl5/vendor_perl -Dvendorarch=/usr/lib/perl5/vendor_perl
				 -Duselargefiles -Dusethreads -Duseshrplib=false -Dd_semctl_semun
				 -Dman1dir=/usr/share/man/man1 -Dman3dir=/usr/share/man/man3
				 -Dinstallman1dir=/usr/share/man/man1 -Dinstallman3dir=/usr/share/man/man3
				 -Dman1ext=1 -Dman3ext=3pm -Dcf_by=sysplan -Ud_csh -Uusedl -Dusenm
				 -Dusemallocwrap)))
	  (make-recipe
	    #:env   '((BUILD_ZLIB . 0)
		      (BUILD_BZIP2 . 0)
		      (BZIP2_LIB . /sysroot/usr/lib)
		      (BZIP2_INCLUDE . /sysroot/usr/include))
	    #:script (execline*
		       (cd ,(conc "perl-" version))
		       (if ((./Configure -des ,@configure-flags)))
		       (if ((backtick -n -D 4 ncpu ((nproc)))
			    (importas -u ncpu ncpu)
			    (make -j $ncpu)))
		       (if ((make DESTDIR=/out install)))
		       (if ((rm -rf /out/usr/share/man)))
		       (find /out -name ".*" -delete))))))))


(define bison
  (let* ((version '3.4.2)
	 (leaf    (remote-archive
		    (conc "https://ftp.gnu.org/gnu/bison/bison-" version ".tar.xz")
		    "mISdVqFsbf8eeMe8BryH2Zf0oxcABOyW26NZT61RMSU=")))
    (package-lambda
      conf
      (make-package
	#:label  (conc "bison-" version "-" (conf 'arch))
	#:src    leaf
	#:tools  (append (list m4 perl) (cc-for-target conf))
	#:inputs (list musl libssp-nonshared)
	#:build  (gnu-build (conc "bison-" version) conf
			    ;; there is a buggy makefile in examples/ that will
			    ;; occasionally explode during parallel builds;
			    ;; just delete the directory entirely
			    ;; https://lists.gnu.org/archive/html/bug-bison/2019-10/msg00044.html
			    #:pre-configure (execline*
					      (if ((rm -rf examples/c/recalc)))))))))

(define flex
  (let* ((version '2.6.4)
	 (leaf    (remote-archive
		    (conc "https://github.com/westes/flex/releases/download/v" version "/flex-" version ".tar.gz")
		    "iyTO0NJ3ype1atS2SbQkeEA24JzbugW3OCxyRQVBBCw=")))
    (package-lambda
      conf
      (make-package
	#:label  (conc "flex-" version "-" (conf 'arch))
	#:src    leaf
	;; TODO: m4 is also a runtime dependency of flex
	#:tools  (cons m4 (cc-for-target conf))
	#:inputs (list musl libssp-nonshared bison)
	#:build  (gnu-build (conc "flex-" version) conf)))))

(define ncurses
  (let* ((version '6.1-20200104)
	 (leaf    (remote-archive
		    (conc "https://invisible-mirror.net/archives/ncurses/current/ncurses-" version ".tgz")
		    "X9iVpAqz8GCEy2eTCYnn3BmLBwbf3nintXEFXNIw8LI="))
	 (extra-flags '(--without-ada --without-tests --disable-termcap --disable-rpath-hack --disable-stripping --without-cxx-binding)))
    (package-lambda
      conf
      (make-package
	#:label  (conc "ncurses-" version "-" (conf 'arch))
	#:src    leaf
	#:tools  (cc-for-target conf)
	#:inputs (list musl libssp-nonshared)
	#:build  (gnu-build (conc "ncurses-" version)
			    ;; XXX gross hack: the ncurses developers,
			    ;; in their infinite wisdom, have decided
			    ;; to break reproducible builds and insist
			    ;; on ARFLAGS=-curvU...
			    (config-prepend conf 'configure-flags extra-flags)
			    #:make-flags '("ARFLAGS=-Dcrv"))))))

(define texinfo
  (let* ((version '6.7)
	 (leaf    (remote-archive
		    (conc "https://ftp.gnu.org/gnu/texinfo/texinfo-" version ".tar.xz")
		    "sRSBGlRp4y484pt7mtcx_xVSIi6brC5ejffXfQ9wInE=")))
    (package-lambda
      conf
      (make-package
	#:label (conc "texinfo-" version "-" (conf 'arch))
	#:src   leaf
	#:tools  (cc-for-target conf)
	#:inputs (list musl libssp-nonshared perl ncurses)
	#:build  (gnu-build (conc "texinfo-" version) conf)))))

(define zlib
  (let* ((version '1.2.11)
	 (leaf    (remote-archive
		    (conc "https://zlib.net/zlib-" version ".tar.gz")
		    "K3Q8ig9qMtClPdpflHwS8OkSMrLVItBzEu5beP_szJA=")))
    (package-lambda
      conf
      (make-package
	#:label (conc "zlib-" version "-" (conf 'arch))
	#:src   leaf
	#:tools (cc-for-target conf)
	#:inputs (list musl libssp-nonshared)
	#:build (gnu-build
		  (conc "zlib-" version) conf
		  ;; not autoconf;
		  ;; stick CFLAGS and friends into the environment
		  #:pre-configure (map (lambda (p)
					 `(export ,(car p) ,(conc "\"" (apply-conc (cdr p)) "\"")))
				       (cons
					 (cons 'CHOST (conf 'arch))
					 (cc-env conf)))
		  #:configure '(--static --prefix=/usr --libdir=/lib))))))

(define libisl
  (let* ((version '0.18)
	 (leaf    (remote-archive
		    (conc "https://gcc.gnu.org/pub/gcc/infrastructure/isl-" version ".tar.bz2")
		    "bFSNjbp4fE4N5xcaqSGTnNfLPVv7QhnEb2IByFGBZUY=")))
    (package-lambda
      conf
      (make-package
	#:label  (conc "isl-" version "-" (conf 'arch))
	#:src    leaf
	#:tools  (cc-for-target conf)
	#:inputs (list libgmp musl libssp-nonshared)
	#:build  (gnu-build (conc "isl-" version) conf)))))

;; patch* creates a series of patch artifacts
;; from a collection of verbatim strings
(define (patch* . patches)
  (if (null? patches)
    '()
    (let loop ((n 0)
	       (head (car patches))
	       (rest (cdr patches)))
      (cons
	(interned (conc "/src/patch-" n ".patch") #o644 head)
	(if (null? rest) '() (loop (+ n 1) (car rest) (cdr rest)))))))

;; script-apply-patches produces the execline expressions
;; for applying a series of patches from artifact files
(define (script-apply-patches lst)
  (map
    (lambda (pf)
      `(if ((patch -p1 "-i" ,(vector-ref (vector-ref pf 0) 1)))))
    lst))

;; include a file as literal text at compile time
(define-syntax include-file-text
  (er-macro-transformer
    (lambda (expr rename cmp)
      (let ((arg (cadr expr)))
	(unless (string? arg)
	  (syntax-error "include-file-text expects a literal string; got" expr))
	(with-input-from-file arg read-string)))))

(define make
  (let* ((version '4.2.1)
	 (leaf    (remote-archive
		    (conc "https://ftp.gnu.org/gnu/make/make-" version ".tar.bz2")
		    "BNFWkQKgF9Vt6NUbdway0KbpUiZJ6PFQDLG4L29xjlg="))
	 (patches (patch*
		    ;; patches are the same ones that Alpine uses
		    ;; in order to fix musl compatibility
		    (include-file-text "patches/make/fix-atexit-exit.patch")
		    (include-file-text "patches/make/fix-glob-dtype.patch"))))
    (package-lambda
      conf
      (make-package
	#:label  (conc "make-" version "-" (conf 'arch))
	#:src    (cons leaf patches)
	#:tools  (cc-for-target conf)
	#:inputs (list musl libssp-nonshared)
	#:build  (gnu-build (conc "make-" version) conf
			    #:pre-configure (script-apply-patches patches))))))

(define binutils-for-target
  (let* ((version '2.33.1)
	 (leaf    (remote-archive
		    (conc "https://ftp.gnu.org/gnu/binutils/binutils-" version ".tar.bz2")
		    "wuwvGNrMYaSLio4yHUAS8qM3-ugn1kqrqEn2M6LcNT0=")))
    (memoize-eq
      (lambda (target)
	(package-lambda
	  host
	  (make-package
	    #:label (conc "binutils-" version "-" (host 'arch) "-" (target 'arch))
	    #:src   leaf
	    #:tools  (append (list bison flex m4) (cc-for-target host))
	    #:inputs (list libisl zlib libgmp libmpfr libmpc musl libssp-nonshared)
	    #:build  (gnu-build (conc "binutils-" version)
				(config-prepend host 'configure-flags
						`(--with-sysroot=/ --with-build-sysroot=/sysroot --target ,(triple target)
								   --disable-multilib --enable-ld=default --enable-gold
								   --enable-64-bit-bfd --enable-relro
								   --enable-deterministic-archives --disable-install-libiberty
								   --enable-default-hash-style=gnu --with-mmap --with-system-zlib)))))))))
