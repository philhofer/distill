(define-syntax package-lambda
  (syntax-rules ()
    ((_ conf body* ...)
     (memoize-eq
       (lambda (conf) body* ...)))))

;; *build-config* is the configuration used
;; to build tools for the build machine
(define *build-config*
  (table->proc
    (table
      `((arch . ,*this-machine*)
	(CFLAGS -pipe -fstack-protector-strong -Os)))))

(define *prebuilts*
  `((x86_64 . ,(include "prebuilt-x86_64.scm"))))

(define (config-builder alist)
  (let ((conf (table->proc (table alist))))
    (lambda (pkg)
      (build-package! pkg *build-config* conf))))

(define (maybe-prebuilt conf ref)
  (and-let* ((_ (eq? conf *build-config*))
	     (c (assq *this-machine* *prebuilts*))
	     (c (assq ref (cdr c))))
    (cdr c)))

(define (bootstrap-base!)
  (let* ((build *build-config*)
	 (target build)
	 ;; this is a bit of a hack: here we get a reference
	 ;; to a global that we set-cdr! on below to override
	 ;; the prebuilts with new prebuilts that we have
	 ;; bootstrapped
	 (built  (assq *this-machine* *prebuilts*))
	 (->boot (lambda (pkg)
		   (lambda (conf)
		     (let ((old (pkg conf)))
		       (unless (package-prebuilt old)
			 (error "expected package to have a prebuilt binary:" (package-label old)))
		       (update-package old #:prebuilt #f)))))
	 (pkgs   `((make . ,(->boot make))
		   (busybox . ,(->boot busybox-core))
		   (execline . ,(->boot execline-tools))
		   (binutils . ,(->boot (binutils-for-target target)))
		   (gcc . ,(->boot (gcc-for-target target)))))
	 (remake! (lambda ()
		    ;; use pkgs to build a new prebuilt alist
		    (map
		      (lambda (p)
			(cons (car p)
			      (build-package! (cdr p) build target)))
		    pkgs)))
	 (stable? (lambda (alist)
		    ;; check if the given alist and built alist are equivalent
		    (let loop ((lst alist))
		      (or (null? lst)
			  (let* ((p    (car lst))
				 (name (car p))
				 (art  (cdr p))
				 (rest (cdr lst)))
			    (and (string=? (artifact-hash art)
					   (artifact-hash (cdr (assq name (cdr built)))))
				 (loop rest))))))))
    (let ((stage-1 (remake!)))
      (if (stable? stage-1)
	(display "prebuilts already equivalent.\n")
	(begin
	  (display "outputting new prebuilts.\n")
	  (with-output-to-file
	    (conc "prebuilt-" *this-machine* ".scm")
	    (lambda ()
	      (write (list 'quote stage-1)))))))))

;; compiler tools should use <arch>-linux-musl as the system triple
(define (triple conf)
  (string->symbol
    (conc (conf 'arch) '-linux-musl)))

(define (sysroot conf)
  (filepath-join "/sysroot/" (triple conf)))

(define (config-prepend conf label lst)
  (let ((val (append lst (or (conf label) '()))))
    (lambda (sym)
      (if (eq? sym label) val (conf sym)))))

(define (cc-for-target conf)
  (list (gcc-for-target conf)
	(binutils-for-target conf)
	make
	execline-tools
	busybox-core))

(define (sysroot-flag conf)
  (string-append "--sysroot=" (sysroot conf)))

;; cc-env takes a configuration and produces an alist
;; with typical configure/make variables set to the
;; appropriate values (CC{FLAGS}, LD{FLAGS}, CXX{FLAGS}, etc.)
(define cc-env
  (memoize-eq
    (lambda (conf)
      (let ((need-cflags    `(,(sysroot-flag conf) -fPIE -static-pie))
	    (need-ldflags   `(,(sysroot-flag conf) -static-pie))
	    (need-cppflags  `(,(sysroot-flag conf)))
	    (cflags          (conf 'CFLAGS))
	    (ldflags         (conf 'LDFLAGS))
	    (c++flags        (or (conf 'CXXFLAGS) (conf 'CFLAGS)))
	    (plat            (triple conf))
	    (join            (lambda (a b)
			       (cond
				 ((eq? a #f) b)
				 ((list? a)  (append a b))
				 (else (cons a b))))))
	`((CC  . ,(conc plat "-gcc"))
	  (AR  . ,(conc plat "-ar"))
	  (LD  . ,(conc plat "-ld"))
	  (AS  . ,(conc plat "-as"))
	  (CXX . ,(conc plat "-g++"))
	  (CBUILD . ,(conc *this-machine* "-linux-musl"))
	  (CHOST  . ,plat)
	  (CFLAGS   . ,(join cflags need-cflags))
	  (CXXFLAGS . ,(join c++flags need-cflags))
	  (LDFLAGS  . ,(join ldflags need-ldflags)))))))

;; cc-env/build produces the environment for CC_FOR_BULID and friends
(define (cc-env/build)
  (let ((need-cflags   '(-fPIE -static-pie))
	(need-ldflags  '(--sysroot=/ -static-pie))
	(plat          (conc *this-machine* "-linux-musl")))
    ;; if we don't set CC_FOR_BUILD to include '--sysroot=/',
    ;; then autoconf CPP (preprocessor) checks fail, because
    ;; they won't include the sysroot flags
    `((CC_FOR_BUILD  gcc --sysroot=/)
      (CXX_FOR_BUILD g++ --sysroot=/)
      (LD_FOR_BUILD  . ,(conc plat "-ld"))
      (AR_FOR_BUILD  . ,(conc plat "-ar"))
      (AS_FOR_BUILD  . ,(conc plat "-as"))
      (RANLIB_FOR_BUILD . ,(conc plat "-ranlib"))
      (STRIP_FOR_BUILD  . ,(conc plat "-strip"))
      (NM_FOR_BUILD     . ,(conc plat "-nm"))
      (CFLAGS_FOR_BUILD   . ,need-cflags)
      (CXXFLAGS_FOR_BUILD . ,need-cflags)
      (LDFLAGS_FOR_BUILD  . ,need-ldflags))))

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

(define (pair->string= p) (conc (car p) "=" (apply-conc (cdr p))))

;; configure-args produces arguments to an autotools configure script
;; NOTE: some packages contain a script called 'configure' that isn't
;; actually generated by libtool, in which case this function may not produce
;; appropriate arguments
(define configure-args
  (memoize-eq
    (lambda (conf)
      (let ((need-conf `(--disable-shared --disable-nls --enable-static --enable-pie --with-pic --prefix=/usr --sysconfdir=/etc --build ,(conc *this-machine* "-linux-musl") --host ,(triple conf)))
	    (usr-conf  (conf 'configure-flags)))
	(cond
	  ((eq? usr-conf #f) need-conf)
	  ((list? usr-conf)  (append usr-conf need-conf))
	  (else              (cons usr-conf need-conf)))))))

(define *musl-version* '1.1.24)
(define *musl-src*    (remote-archive
			(conc "https://www.musl-libc.org/releases/musl-" *musl-version* ".tar.gz")
			"hC6Gf8nyJQAZVYJ-tNG0iU0dRjES721p0x1cqBp2Ge8="))

;; when building a cross-compiler,
;; we need headers for the target system
;;
;; this package is a hack that provides a 'tool'
;; that makes libc headers appear at a particular sysroot
(define musl-headers-for-target
  (memoize-eq
    (lambda (target)
      (memoize-eq
	(lambda (host)
	  (let* ()
	    (make-package
	      #:label (conc "musl-headers-" *musl-version* "-" (target 'arch))
	      #:src   *musl-src*
	      #:tools (list make execline-tools busybox-core)
	      #:inputs '()
	      #:build (make-recipe
			#:script (execline*
				   (cd ,(conc "musl-" *musl-version*))
				   (make ,(conc "DESTDIR=/out/" (sysroot target)) ,(conc "ARCH=" (target 'arch)) "prefix=/usr" install-headers))))))))))

(define musl
  (let ()
    ;; note: musl is compiled as -ffreestanding, so
    ;; the gcc that builds it does *not* need to know
    ;; how to find a libc.a or crt*.o, and so forth
    (package-lambda
      conf
      (make-package
	#:label  (conc "musl-" *musl-version* "-" (conf 'arch))
	#:src    *musl-src*
	#:tools  (cc-for-target conf)
	#:inputs '()
	#:build (make-recipe
		  ;; ./configure, but not autotools
		  #:script (let* ((cenv   (cc-env conf))
				  (CC     (assq 'CC cenv))
				  (CFLAGS (assq 'CFLAGS cenv)))
			     (execline*
			       (cd ,(conc "musl-" *musl-version*))
			       (if ((./configure --disable-shared --enable-static
						 --prefix=/usr ,(pair->string= CC)
						 ,(pair->string= CFLAGS)
						 --target ,(conf 'arch))))
			       (if ((backtick -n -D 4 ncpu ((nproc)))
				    (importas -u ncpu ncpu)
				    (make -j $ncpu ,@(makeflags conf))))
			       (make DESTDIR=/out install))))))))

(define libssp-nonshared
  (package-lambda
    conf
    (make-package
	#:label   (conc "libssp-nonshared-" (conf 'arch))
	#:tools   (cc-for-target conf)
	#:inputs  '()
	#:src     (interned "/src/ssp-nonshared.c" #o644 #<<EOF
extern void __stack_chk_fail(void);
void __attribute__((visibility ("hidden"))) __stack_chk_fail_local(void) { __stack_chk_fail(); }

EOF
)
	#:build (make-recipe
		  #:script (let* ((cenv (cc-env conf))
				  (CC   (cdr (assq 'CC cenv)))
				  (AR   (cdr (assq 'AR cenv))))
			     (execline*
			       (cd ./src)
			       (if ((,CC -c -fPIE -Os ssp-nonshared.c -o __stack_chk_fail_local.o)))
			       (if ((,AR -Dcr libssp_nonshared.a __stack_chk_fail_local.o)))
			       (if ((mkdir -p /out/usr/lib)))
			       (cp libssp_nonshared.a /out/usr/lib/libssp_nonshared.a)))))))

;; dependencies necessary to statically link an ordinary C executable
(define libc (list musl libssp-nonshared))

(define (export* alist)
  (map (lambda (p)
	 `(export ,(car p) ,(apply-conc (cdr p))))
       alist))

;; we don't use tools just name 'ar' etc.
;; because they would conflict if multiple
;; binutils targets were installed
;;
;; let make know what we call these tools explicitly
(define (make-env target)
  (let ((name (triple target)))
    (list
      (cons 'AR      (conc name "-ar"))
      (cons 'RANLIB  (conc name "-ranlib"))
      (cons 'STRIP   (conc name "-strip"))
      (cons 'READELF (conc name "-readelf"))
      (cons 'OBJCOPY (conc name "-objcopy"))
      (cons 'ARFLAGS '-Dcr))))

(define (makeflags target) (map pair->string= (make-env target)))

(define (+cross conf normal extra)
  (if (eq? (conf 'arch) *this-machine*)
    normal
    (append extra normal)))

;; generator for an execline sequence that strips binaries
(define (strip-binaries-script target)
  (execline*
    (forbacktickx file ((find /out -type f -perm -o+x)))
    (importas "-i" -u file file)
    (backtick prefix ((head -c4 $file)))
    (importas "-i" -u prefix prefix)
    ;; the execline printing code knows
    ;; how to escape a raw byte sequence
    ;; (elf binaries begin with (0x7f)ELF)
    (if ((test $prefix "=" #u8(127 69 76 70))))
    (if ((echo "strip" $file)))
    (,(conc (triple target) "-strip") $file)))

;; wrapper around the 'configure;make;make install' pattern,
;; taking care to set configure flags make flags appropriately
;; for the common case that we're dealing with autotools
(define (gnu-build dir target #!key
		   (pre-configure '())
		   (post-install '())
		   (make-flags '())
		   (out-of-tree #f) ;; build out of tree
		   (configure #f))  ;; alternate configure flags
  (make-recipe
    #:script (execline*
	       (cd ,dir)
	       ,@pre-configure
	       ,@(export* (cc-env target))
	       ;; if we're doing an out-of-tree build,
	       ;; move the rest of the build to /builddir
	       ,@(if out-of-tree
		   `((if ((mkdir -p "/builddir")))
		     (cd "/builddir")
		     (if ((,(conc "/" dir "/configure")
			   ,@(or configure (configure-args target))))))
		   `((if ((./configure ,@(or configure (configure-args target)))))))
	       ;; it's helpful (mandatory?) that the script not change
	       ;; based on the number of cpus on the host, so we have
	       ;; to make that determination in the script itself
	       (if ((backtick -n -D 4 ncpu ((nproc)))
		    (importas -u ncpu ncpu)
		    (make -j $ncpu ,@(append
				       (makeflags target)
				       make-flags))))
	       (if ((make DESTDIR=/out install)))
	       ,@post-install
	       ;; we don't care if these two succeed
	       ;; TODO: perhaps gnu packages should
	       ;; symlink makeinfo to /usr/bin/true
	       (foreground ((rm -rf /out/usr/share/man)))
	       (foreground ((rm -rf /out/usr/share/info)))
	       ;; the presence of libtool archives in library output
	       ;; actually breaks builds, because libtool doesn't
	       ;; undestand --sysroot, etc
	       (foreground ((find /out -type f -name "*.la" -delete)))
	       ,@(strip-binaries-script target))))

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
	#:inputs libc
	#:build  (gnu-build (conc "gawk-" version)
			    conf
			    #:post-install
			    ;; we don't want the awk symlink;
			    ;; it conflicts with busybox
			    (execline*
			      (foreground ((rm -f /out/usr/bin/awk)))))))))

(define (native-toolchain)
  (list libssp-nonshared
	musl
	native-gcc
	native-binutils))

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
	#:tools  (+cross
		   conf (cons m4 (cc-for-target conf))
		   (native-toolchain))
	#:inputs libc
	#:build  (gnu-build (conc "gmp-" version) conf
			    #:pre-configure
			    (+cross conf
			      '()
			      (export*
				'((CC_FOR_BUILD gcc -fPIE -static-pie --sysroot=/)))))))))

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
	#:inputs (cons libgmp libc)
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
	#:inputs (cons* libgmp libmpfr libc)
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
	#:inputs libc
	#:build  (gnu-build (conc "m4-" version) conf
			    ;; m4 sticks a file in /usr/lib/charset.alias
			    #:post-install (execline*
					     (if ((rm -rf /out/usr/lib)))))))))

(define bzip2
  (let* ((version '1.0.8)
	 (leaf    (remote-archive
		    (conc "https://sourceware.org/pub/bzip2/bzip2-" version ".tar.gz")
		    "pZGXjBOF4VYQnwdDp2UYObANElrjShQaRbMDj5yef1A=")))
    (package-lambda
      conf
      (make-package
	#:label  (conc "bzip2-" version "-" (conf 'arch))
	#:src    leaf
	#:tools  (cc-for-target conf)
	#:inputs libc
	#:build  (make-recipe
		   #:script (execline*
			      (cd ,(conc "bzip2-" version))
			      (if ((make ,@(map pair->string= (cc-env conf))
					 ,@(makeflags conf)
					 all)))
			      (make PREFIX=/out/usr install)))))))

(define (ska-build dir conf #!key (extra-configure '()))
  (let ((cenv    (cc-env conf))
	(sysrt   (sysroot conf)))
    (make-recipe
      #:script (execline*
		 (cd ,dir)
		 ,@(export* cenv)
		 (if ((sed "-i" -e "/^tryflag.*-fno-stack/d" -e "s/^CFLAGS=.*$/CFLAGS=/g" configure)))
		 (if ((./configure --target ,(triple conf) --prefix=/ --libdir=/usr/lib
				   ,(conc "--with-include=" (filepath-join sysrt "/include"))
				   ,(conc "--with-include=" (filepath-join sysrt "/usr/include"))
				   ,(conc "--with-lib=" (filepath-join sysrt "/lib"))
				   ,(conc "--with-lib=" (filepath-join sysrt "/usr/lib"))
				   --disable-shared --enable-static ,@extra-configure)))
		 (if ((backtick -n -D 4 ncpu ((nproc)))
		      (importas -u ncpu ncpu)
		      (make -j $ncpu ,@(makeflags conf))))
		 (if ((make DESTDIR=/out install)))
		 ,@(strip-binaries-script conf)))))

(define skalibs
  (let* ((version '2.9.1.0)
	 (leaf    (remote-archive
		    (conc "https://skarnet.org/software/skalibs/skalibs-" version ".tar.gz")
		    "FlrvgEOHU_UzTJDBjUrQ0HHLijHeqstC_QgCK-NE2yo=")))
    (package-lambda
      conf
      (make-package
	#:label  (conc "skalibs-" version "-" (conf 'arch))
	#:src    leaf
	#:tools  (cc-for-target conf)
	#:inputs libc
	#:build  (ska-build (conc "skalibs-" version) conf
			    #:extra-configure '(--with-sysdep-devurandom=yes))))))

(define execline-tools
  (let* ((version '2.5.3.0)
	 (leaf    (remote-archive
		    (conc "https://skarnet.org/software/execline/execline-" version ".tar.gz")
		    "qoNVKJ4tBKdrzqlq10C5rqIsv3FDzeFa9umqs7EJOdM=")))
    (package-lambda
      conf
      (make-package
	#:prebuilt (maybe-prebuilt conf 'execline)
	#:label    (conc "execline-" version "-" (conf 'arch))
	#:src      leaf
	#:tools    (cc-for-target conf)
	#:inputs   (cons skalibs libc)
	#:build    (ska-build (conc "execline-" version) conf
			      #:extra-configure `(,(conc "--with-sysdeps=" (sysroot conf) "/lib/skalibs/sysdeps") --enable-static-libc))))))


;; byacc is a yacc(1) implementation that is much simpler than bison
(define byacc
  (let* ((version '20191125)
	 (leaf    (remote-archive
		    ;; XXX this isn't a stable tarball path;
		    ;; this will fail when upstream changes
		    "https://invisible-island.net/datafiles/release/byacc.tar.gz"
		    "2r0VA-wLi8PcDpjnyON2pyyzqY7a7tdfApRBa-HfYbg=")))
    (package-lambda
      conf
      (make-package
	#:label  (conc "byacc-" version "-" (conf 'arch))
	#:src    leaf
	#:tools  (cc-for-target conf)
	#:inputs libc
	#:build  (gnu-build (conc "byacc-" version) conf)))))

;; reflex is a lex(1)+flex(1) implementation that is much simpler
;; than GNU flex(1)
(define reflex
  (let* ((version '20191123)
	 (leaf    (remote-archive
		    "https://invisible-island.net/datafiles/release/reflex.tar.gz"
		    "SsgYKYUlYwedhJWxTvBAO2hdfrAMJ8mNpFjuIveGpSo=")))
    (package-lambda
      conf
      (make-package
	#:label   (conc "reflex-" version "-" (conf 'arch))
	#:src     leaf
	#:tools   (cons byacc (cc-for-target conf))
	#:inputs  libc
	#:build   (gnu-build (conc "reflex-" version) conf
			     #:post-install ;; install the lex(1)+flex(1) symlinks
			     (execline*
			       (if ((ln -s reflex /out/usr/bin/lex)))
			       (if ((ln -s reflex++ /out/usr/bin/lex++)))
			       (if ((ln -s reflex /out/usr/bin/flex)))
			       (if ((ln -s reflex++ /out/usr/bin/flex++)))))))))

(define zlib
  (let* ((version '1.2.11)
	 (leaf    (remote-archive
		    (conc "https://zlib.net/zlib-" version ".tar.gz")
		    "K3Q8ig9qMtClPdpflHwS8OkSMrLVItBzEu5beP_szJA=")))
    (package-lambda
      conf
      (make-package
	#:label  (conc "zlib-" version "-" (conf 'arch))
	#:src    leaf
	#:tools  (cc-for-target conf)
	#:inputs libc
	#:build  (gnu-build
		   ;; not autoconf
		   (conc "zlib-" version) conf
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
	#:inputs (cons libgmp libc)
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
	#:prebuilt (maybe-prebuilt conf 'make)
	#:label    (conc "make-" version "-" (conf 'arch))
	#:src      (cons leaf patches)
	#:tools    (cc-for-target conf)
	#:inputs   libc
	#:build    (gnu-build (conc "make-" version) conf
			      #:pre-configure (script-apply-patches patches))))))

(define *binutils-version* '2.33.1)
(define *gcc-version* '9.2.0)

(define *binutils-src*
  (remote-archive
    (conc "https://ftp.gnu.org/gnu/binutils/binutils-" *binutils-version* ".tar.bz2")
    "wuwvGNrMYaSLio4yHUAS8qM3-ugn1kqrqEn2M6LcNT0="))

(define *gcc-src*
  (remote-archive
    (conc "https://gcc.gnu.org/pub/gcc/releases/gcc-" *gcc-version* "/gcc-" *gcc-version* ".tar.gz")
    "nzv8PqT49a1fkkQtn60mgKbxtcTl8Mazq0swhypLLRo="))

(define (gcc-target-flags conf)
  (case (conf 'arch)
    ((aarch64) '(--with-arch=armv8 --with-abi=lp64))
    ((ppc64 ppc64le) '(--with-abi=elfv2 --enable-secureplt
					--enable-decimal-float=no
					--enable-targets=powerpcle-linux))
    (else '())))

(define binutils-for-target
  (memoize-eq
    (lambda (target)
      (memoize-eq
	(lambda (host)
	  (let ((host-arch     (host 'arch))
		(target-arch   (target 'arch))
		(target-triple (triple target))
		(host-triple   (triple host))
		(build-triple  (conc *this-machine* "-linux-musl"))
		(target-sysrt  (sysroot target)))
	    (make-package
	      #:prebuilt (and (eq? host target) (maybe-prebuilt host 'binutils))
	      #:label   (conc "binutils-" (target 'arch) "-"
			      (if (eq? host-arch target-arch)
				"native"
				host-arch))
	      #:src     *binutils-src*
	      #:tools   (let ((t (cons* byacc reflex (cc-for-target host))))
			  (if (eq? host-arch target-arch)
			    (if (eq? *this-machine* host-arch)
			      t ;; libc headers are already there; it's the host sysroot
			      (append (native-toolchain) t))
			    (cons (musl-headers-for-target target) t)))
	      #:inputs  (cons zlib libc)
	      #:build
	      (let ()
		(gnu-build
		  (conc "binutils-" *binutils-version*)
		  host
		  #:pre-configure (if (eq? *this-machine* host-arch)
				    '()
				    (export* (cc-env/build)))
		  #:configure `(--disable-nls --disable-shared --enable-static
				--disable-multilib --enable-gold=yes --with-ppl=no
				--disable-install-libiberty --enable-relro
				--disable-plugins --enable-deterministic-archives
				--with-pic --with-mmap --enable-ld=default
				--with-system-zlib --enable-64-bit-bfd
				--disable-install-libbfd
				--prefix=/usr
				;; no libdir, etc. because we discard any
				;; libraries and headers produced
				,(conc "--program-prefix=" target-triple "-")
				,(conc "--build=" build-triple)
				,(conc "--target=" target-triple)
				,(conc "--host=" host-triple)
				,(conc "--with-sysroot=" target-sysrt))
		  #:post-install (execline*
				   (if ((rm -rf /out/usr/include)))
				   (if ((rm -rf /out/include)))
				   (if ((rm -rf /out/usr/lib)))
				   (if ((rm -rf /out/lib)))))))))))))

;; this is a hack that allows us to build a cross-gcc
;; without circular dependencies: install a fake set of
;; object files that define the same symbols that musl does
(define fake-musl-for-target
  (memoize-eq
    (lambda (target)
      (memoize-eq
	(lambda (host)
	  (let* ((hash "GpFZDYJsPUIYcsfZe-22Qk90kJ9albSjYSf_qTfzuuA=")
		 (leaf (remote-archive
			 (conc "https://b2cdn.sunfi.sh/file/pub-cdn/" hash)
			 hash #:kind 'tar.zst))
		 (version '0.1))
	    (make-package
	      #:label (conc "fakemusl-" version "-" (host 'arch) "-" (target 'arch))
	      #:src   leaf
	      #:tools (list (binutils-for-target target)
			    make
			    execline-tools
			    busybox-core)
	      #:inputs '()
	      #:build (let* ((tool      (triple target))
			     (target-as (conc tool "-as"))
			     (target-ar (conc tool "-ar"))
			     (outdir    (filepath-join "/out" (sysroot target))))
			(make-recipe
			  #:script (execline*
				     (cd ,(conc "fakemusl-" version))
				     (if ((backtick -D 4 -n jflag ((nproc)))
					  (importas -u jflag jflag)
					  (make -j $jflag
						,(conc "AS=" target-as)
						,(conc "AR=" target-ar)
						all)))
				     (make PREFIX=/usr ,(conc "DESTDIR=" outdir) install)))))))))))

(define gcc-for-target
  (memoize-eq
    (lambda (target)
      (memoize-eq
	(lambda (host)
	  (let ((target-arch   (target 'arch))
		(host-arch     (host 'arch))
		(target-triple (triple target))
		(host-triple   (triple host))
		(build-triple  (conc *this-machine* "-linux-musl"))
		(target-sysrt  (sysroot target))
		(patches       (patch*
				 (include-file-text "patches/gcc/pie-gcc.patch"))))
	    (make-package
	      #:prebuilt (and (eq? host target) (maybe-prebuilt host 'gcc))
	      #:label (conc "gcc-" (target 'arch) "-"
			    (if (eq? host-arch target-arch)
			      "native"
			      host-arch))
	      #:src   (cons *gcc-src* patches)
	      ;; this is kinda hairy because we need up to three (!)
	      ;; different toolchains available (build, host, target)
	      #:tools (let ((t (cons* byacc reflex gawk (cc-for-target host))))
			(if (eq? host-arch target-arch)
			  (if (eq? *this-machine* host-arch)
			    t
			    (append (native-toolchain) t))
			  (cons*
			    (binutils-for-target target)
			    (fake-musl-for-target target)
			    (musl-headers-for-target target) t)))
	      #:inputs (list libgmp libmpfr libmpc libisl zlib musl)
	      #:build
	      (let ()
		(gnu-build
		  (conc "gcc-" *gcc-version*)
		  ;; this is a hack to ensure that
		  ;; the gcc driver program always behaves like a cross-compiler
		  (config-prepend host 'CFLAGS '(-DCROSS_DIRECTORY_STRUCTURE))
		  #:out-of-tree (or (not (eq? host-arch target-arch))
				    (not (eq? *this-machine* host-arch)))
		  #:pre-configure (append
				    (script-apply-patches patches)
				    (execline*
				      ;; some makefile templates don't set AR+ARFLAGS correctly;
				      ;; just let them take the values from the environment
				      (if ((find "." -name Makefile.in -exec sed "-i"
						 -e "/^AR = ar/d"
						 -e "/^ARFLAGS = cru/d"
						 "{}" ";")))
				      ;; don't pull in GNU tar just to install headers;
				      ;; force the makefile to use busybox cpio instead
				      (if ((sed "-i"
						-e "s/=install-headers-tar/=install-headers-cpio/g"
						gcc/config.build))))
				    (export* (cons*
					       ;; hacks because gcc doesn't respect
					       ;; pie-by-default builds
					       '(gcc_cv_no_pie . no)
					       '(gcc_cv_c_no_pie . no)
					       '(gcc_cv_c_no_fpie . no)
					       (append
						 (if (eq? *this-machine* host-arch)
						   '()
						   (cc-env/build))
						 (make-env host)))))
		  #:configure `(--prefix=/usr --exec-prefix=/usr --disable-lto
				--disable-nls --disable-shared --enable-static
				--disable-host-shared --enable-host-static
				--disable-multilib --enable-default-ssp
				--disable-bootstrap --disable-libssp
				--enable-default-pie --with-cloog=no
				--with-ppl=no --disable-libquadmath
				--disable-libgomp --disable-fixed-point
				--enable-__cxa_atexit --disable-libstdcxx-pch
				--disable-libmpx --disable-libsanitizer
				--enable-libstdcxx-time --disable-libitm
				--enable-threads --enable-tls
				--disable-install-libiberty
				--enable-relro --disable-plugins
				--with-pic --with-mmap --disable-symvers
				--enable-version-specific-runtime-libs
				--with-system-zlib
				"--enable-languages=c,c++"
				,(conc "--with-sysroot=" (sysroot target))
				,(conc "--build=" build-triple)
				,(conc "--target=" target-triple)
				,(conc "--host=" host-triple))
		  ;; installing compilers for different targets will
		  ;; conflict unless we limit the python gdb stuff
		  ;; to just "native" (host=target) gcc builds
		  #:post-install (if (eq? host-arch target-arch)
				   '()
				   `((if ((rm -rf ,(conc "/out/usr/share/gcc-" *gcc-version* "/python")))))))))))))))

(define native-gcc
  (lambda (build)
    ((gcc-for-target build) build)))

(define native-binutils
  (lambda (build)
    ((binutils-for-target build) build)))

(define busybox-core
  (let* ((version '1.31.1)
	 (leaf    (remote-archive
		    (conc "https://busybox.net/downloads/busybox-" version ".tar.bz2")
		    "JqkfZAknGWBuXj1sPLwftVaH05I5Hb2WusYrYuO-sJk=")))
    (package-lambda
      conf
      ;; NOTE: the busybox config we've selected here is carefully
      ;; chosen so as not to require any linux headers; otherwise
      ;; we'd have to bring in a kernel source tree and perl (shudders)
      (let* ((small-config (interned "/src/config.head" #o644
				     (include-file-text "patches/busybox/config-small")))
	     (cenv         (cc-env conf))
	     (patches      (patch*
			     (include-file-text "patches/busybox/busybox-bc.patch")))
	     (make-flags   (map
			     pair->string=
			     `((CROSS_COMPILE . ,(conc (triple conf) "-"))
			       (CONFIG_SYSROOT . ,(sysroot conf))
			       (CONFIG_EXTRA_CFLAGS . ,(cdr (assq 'CFLAGS cenv)))
			       (HOSTCC    . gcc)
			       (HOSTCFLAGS  --sysroot=/ -fPIE -static-pie)
			       (HOSTLDFLAGS --sysroot=/ -static-pie)))))
	(make-package
	  #:prebuilt (maybe-prebuilt conf 'busybox)
	  #:label  (conc "busybox-core-" version "-" (conf 'arch))
	  #:src    (cons* small-config leaf patches)
	  #:tools  (cons bzip2
			 (append
			   libc (cc-for-target conf)
			   ;; add a native toolchain if it isn't already implied
			   (if (eq? (conf 'arch) *this-machine*)
			     '()
			     (list native-gcc native-binutils))))
	  #:inputs libc
	  #:build  (make-recipe
		     #:script (execline*
				(cd ,(conc "busybox-" version))
				(export KCONFIG_NOTIMESTAMP 1)
				,@(script-apply-patches patches)
				(if ((mv /src/config.head .config)))
				(if ((backtick -n -D 1 jflag ((nproc)))
				     (importas -u jflag jflag)
				     (make V=1 -j $jflag ,@make-flags busybox)))
				(if ((make V=1 busybox.links)))
				(if ((install -D -m "755" busybox /out/bin/busybox)))
				(if ((mkdir -p /out/usr/bin /out/sbin /out/usr/sbin)))
				(redirfd -r 0 busybox.links)
				(forstdin -o 0 link)
				(importas "-i" -u link link)
				(ln -s /bin/busybox "/out/${link}"))))))))
