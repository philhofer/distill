(define-syntax package-lambda
  (syntax-rules ()
    ((_ conf body* ...)
     (memoize-eq
       (lambda (conf) body* ...)))))

;; pkgs->bootstrap takes a list of package-lambdas
;; and overrides their src and tools so that they
;; don't depend on any tools packages, and instead
;; use the tools provided by a bootstrap tarball
;; that is unpacked at / before building
(define (pkgs->bootstrap . pkgs)
  (let ((ht (make-hash-table)))
    (define (inner pkg)
      (or (hash-table-ref/default ht pkg #f)
	  (let ((newproc (memoize-eq
			   (lambda (conf)
			     (let ((old (pkg conf)))
			       (update-package
				 old
				 #:label (conc (package-label old) "-bootstrap")
				 #:src   (flatten (bootstrap-tools (conf 'arch)) (package-src old))
				 #:tools '()
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
  (lambda (conf)
    (error (conc "package" name "not implemented yet"))))

(define (gcc-for-target conf)
  (placeholder "gcc"))

(define (binutils-for-target conf)
  (placeholder "binutils"))

(define busybox (placeholder "busybox"))

(define (cc-for-target conf)
  (list (gcc-for-target conf) (binutils-for-target conf) busybox))

(define bootstrap-tools
  (let ((x86-64 (local-archive
		  'tar.xz
		  "FUpEL6WVF4U2PorVG_Pt8AX4swLbyiYmlU46k5EVj4c=")))
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
      (let ((need-cflags    '(--sysroot=/sysroot -fPIC -static-pie))
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
	     (need-conf `(--disable-shared --disable-nls --enable-static --enable-pie --prefix=/usr --sysconfdir=/etc --with-sysroot=/sysroot --host ,(conf 'arch)))
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
			     (if ((gcc -c -Os ssp-nonshared.c -o __stack_chk_fail_local.o)))
			     (if ((ar r libssp_nonshared.a __stack_chk_fail_local.o)))
			     (if ((mkdir -p /out/usr/lib)))
			     (cp libssp_nonshared.a /out/usr/lib/libssp_nonshared.a))))))

(define musl
  (let* ((version '1.1.24)
	 (leaf    (remote-archive
		    (conc "https://www.musl-libc.org/releases/musl-" version ".tar.gz")
		    "hC6Gf8nyJQAZVYJ-tNG0iU0dRjES721p0x1cqBp2Ge8=")))
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
			     (if ((./configure --disable-shared --enable-static --prefix=/usr ,(pair->quoted-string (cons 'CFLAGS (or (conf 'CFLAGS) ""))) --target ,(conf 'arch))))
			     (if ((backtick -n -D 4 ncpu ((nproc)))
				  (importas -u ncpu ncpu)
				  (make -j $ncpu)))
			     (make DESTDIR=/out install)))))))

;; wrapper around the 'configure;make;make install' pattern,
;; taking care to set configure flags make flags appropriately
;; for the common case that we're dealing with autotools
(define (gnu-build dir target #!key (pre-configure '()) (post-install '()) (configure #f))
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
		    (make -j $ncpu)))
	       ,@(if (null? post-install)
		     '((make DESTDIR=/out install))
		     (cons '(if ((make DESTDIR=/out install)))
			   post-install)))))

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
	#:tools  (cc-for-target conf)
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
		    "_Zto8BBAes0pngDpz96kt5-VLF6oA0wVmLGqAVBdHd0Y")))
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

(define perl (placeholder "perl"))

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
	#:build  (gnu-build (conc "bison-" version) conf)))))

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
	#:tools  (cons bison (cc-for-target conf))
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
			    (config-prepend conf 'configure-flags extra-flags))))))

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

