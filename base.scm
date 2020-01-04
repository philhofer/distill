(import
  (chicken pretty-print)
  (log)
  (execline)
  (plan))

(define-syntax defpkg
  (syntax-rules ()
    ((_ label ((k v) ...) body* ...)
     (define label
       (memoize-eq
	 (lambda (conf)
	   (let ((k (conf (quote v))) ...)
	     (make-package
	       #:label (symbol->string (quote label))
	       body* ...))))))))

(define-syntax package-lambda
  (syntax-rules ()
    ((_ conf body* ...)
     (memoize-eq
       (lambda (conf) body* ...)))))

(define (package-transform pkg . procs)
  (memoize-eq
    (lambda (conf)
      (let loop ((p     pkg)
		 (xfrms procs))
	(if (null? xfrms)
	    p
	    (loop ((car xfrms) p) (cdr xfrms)))))))

(define (pkg/replace-input from to)
  (define (replace from to lst)
    (if (null? lst)
	lst
	(let ((head (car lst))
	      (rest (cdr lst)))
	  (if (eq? head from)
	      (cons to rest)
	      (let ((tail (replace from to rest)))
		(if (eq? tail rest)
		    lst
		    (cons head tail)))))))
  (lambda (pkg)
    (update-package
      pkg
      #:inputs (replace from to (package-inputs pkg)))))

(define (pkg/replace-label v)
  (lambda (pkg)
    (update-package
      pkg
      #:label v)))

(define (pkg/add-env alist)
  (lambda (pkg)
    (update-package
      pkg
      #:build (let ((r (package-build pkg)))
		(update-recipe
		  r
		  #:env (append (recipe-env r) alist))))))

;; pkg+override wraps a package-lambda
;; with configuration overrides provided
;; as an alist
(define (pkg+override pkg-proc alist)
  (memoize-eq
    (lambda (conf)
      (pkg-proc (lambda (v)
		  (let ((p (memq alist v)))
		    (if p (cdr p) (conf v))))))))

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

(define *default-configure-flags*
  '(--disable-shared --disable-doc --disable-nls --prefix=/usr --sysconfdir=/etc --with-sysroot=/sysroot))

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
			     (if ((./configure ,@*default-configure-flags* ,(string-append "\"CFLAGS=-fstack-protector-strong -Os\"") --target ,(conf 'arch))))
			     (if ((make)))
			     (make DESTDIR=/out install)))))))

(define *CFLAGS* "--sysroot=/sysroot -Os -fstack-protector-strong -fPIC")

(define (gnu-build dir target)
  (make-recipe
    #:script (execline*
	       (cd ,dir)
	       (if ((./configure ,@*default-configure-flags* "\"LDFLAGS=--sysroot=/sysroot -static\"" ,(string-append "\"CFLAGS=" *CFLAGS* "\"") "\"CPPFLAGS=--sysroot=/sysroot\"" --enable-static --enable-pie --host ,(target 'arch))))
	       (if ((make)))
	       (make DESTDIR=/out install))))

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
	#:build  (gnu-build (conc "gawk-" version) (conf 'arch))))))

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

(define m4 (placeholder "m4"))
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

(let* ((conf (table '((arch . x86_64))))
       (host (table->proc conf))
       (target host))
  (parameterize ((artifact-dir "./artifacts")
		 (plan-dir     "./plans")
		 (trace-log    #t))
    (for-each
      (cut build-package! <> host target)
      (pkgs->bootstrap bison libmpc))))
