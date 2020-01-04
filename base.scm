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

(define *musl-version* '1.1.24)
(define *musl-src-url* (fmt #f (cat "https://www.musl-libc.org/releases/musl-" *musl-version* ".tar.gz")))
(define *musl-src-hash* "hC6Gf8nyJQAZVYJ-tNG0iU0dRjES721p0x1cqBp2Ge8=")

(define *musl-src-leaf*
  (remote-archive *musl-src-url* *musl-src-hash*))

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
(defpkg libssp-nonshared-stage1 ((arch arch))
	#:src (list (bootstrap-tools arch))
	#:tools '()
	#:inputs '()
	#:overlay (interned "/src/ssp-nonshared.c" #o644 #<<EOF
extern void __stack_chk_fail(void);
void __attribute__((visibility ("hidden"))) __stack_chk_fail_local(void) { __stack_chk_fail(); }

EOF
)
	#:build (make-recipe
		  #:script (execline*
			     (cd ./src)
			     (if ((gcc -c -Os ssp-nonshared.c -o __stack_chk_fail_local.o)))
			     (if ((ar r libssp_nonshared.a __stack_chk_fail_local.o)))
			     (if ((mkdir -p /out/usr/lib)))
			     (cp libssp_nonshared.a /out/usr/lib/libssp_nonshared.a))))

;; musl built with only bootstrap libraries
;;
(defpkg musl-stage1 ((arch arch))
	#:src  (list (bootstrap-tools arch) *musl-src-leaf*)
	#:tools '() 
	#:inputs '()
	#:build (make-recipe
		  #:script (execline*
			     (cd ,(string-append "musl-" (symbol->string *musl-version*)))
			     (if ((./configure ,@*default-configure-flags* ,(string-append "\"CFLAGS=-fstack-protector-strong -Os\"") --target ,arch)))
			     (if ((make)))
			     (make DESTDIR=/out install))))

(define *CFLAGS* "--sysroot=/sysroot -Os -fstack-protector-strong -fPIC")

(define *bootstrap-c-env*
  `((CFLAGS . ,*CFLAGS*)
    (CC . gcc)))

(define (gnu-build dir target)
  (make-recipe
    #:script (execline* 
	       (cd ,dir)
	       (if ((./configure ,@*default-configure-flags* "\"LDFLAGS=--sysroot=/sysroot -static\"" ,(string-append "\"CFLAGS=" *CFLAGS* "\"") "\"CPPFLAGS=--sysroot=/sysroot\"" --enable-static --enable-pie --host ,target)))
	       (if ((make)))
	       (make DESTDIR=/out install))))

(define *gawk-version* '5.0.1)
(define *gawk-src-leaf*
  (remote-archive
    (fmt #f (cat "https://ftp.gnu.org/gnu/gawk/gawk-" *gawk-version* ".tar.xz"))
    "R3Oyp6YDumBd6v06rLYd5U5vEOpnq0Ie1MjwINSmX-4="))

;; gawk built with musl-stage1 (necessary for building gcc and some of its dependencies)
(defpkg gawk-stage1 ((arch arch))
	#:src (list (bootstrap-tools arch) *gawk-src-leaf*)
	#:tools '()
	#:inputs (list musl-stage1 libssp-nonshared-stage1)
	#:build  (gnu-build (conc "gawk-" *gawk-version*) arch))

(define *libgmp-version* '6.1.2)
(define *libgmp-leaf*
  (remote-archive
    (fmt #f (cat "https://gmplib.org/download/gmp/gmp-" *libgmp-version* ".tar.xz"))
    "bodUs2nnuExA5OmrihkTLOR9P-4cmpWyeyGYiFUPM8s="))

(defpkg libgmp-stage1 ((arch arch))
	#:src   (list (bootstrap-tools arch) *libgmp-leaf*)
	#:tools '()
	#:inputs (list musl-stage1 libssp-nonshared-stage1)
	#:build  (gnu-build (conc "gmp-" *libgmp-version*) arch))

(define *libmpfr-version* '4.0.2)
(define *libmpfr-leaf*
  (remote-archive
    (fmt #f (cat "https://www.mpfr.org/mpfr-current/mpfr-" *libmpfr-version* ".tar.bz2"))
    "wKuAJV_JEeh560Jgqo8Iub6opUuqOKFfQATGEJ2F3ek="))

(defpkg libmpfr-stage1 ((arch arch))
	#:src (list (bootstrap-tools arch) *libmpfr-leaf*)
	#:tools '()
	#:inputs (list musl-stage1 libssp-nonshared-stage1 libgmp-stage1)
	#:build  (gnu-build (conc "mpfr-" *libmpfr-version*) arch))

(define *libmpc-version* '1.1.0)
(define *libmpc-leaf*
  (remote-archive
    (fmt #f (cat "https://ftp.gnu.org/gnu/mpc/mpc-" *libmpc-version* ".tar.gz"))
    "2lH9nuHFlFtOyT_jc5k4x2CHCtir_YwwX9mg6xoGuTc="))

(defpkg libmpc-stage1 ((arch arch))
	#:src    (list (bootstrap-tools arch) *libmpc-leaf*)
	#:tools  '()
	#:inputs (list musl-stage1 libssp-nonshared-stage1 libgmp-stage1 libmpfr-stage1)
	#:build  (gnu-build (conc "mpc-" *libmpc-version*) arch))

(define *bison-version* '3.4.2)
(define *bison-leaf*
  (remote-archive
    (fmt #f (cat "https://ftp.gnu.org/gnu/bison/bison-" *bison-version* ".tar.xz"))
    "mISdVqFsbf8eeMe8BryH2Zf0oxcABOyW26NZT61RMSU="))

(defpkg bison-stage1 ((arch arch))
	#:src    (list (bootstrap-tools arch) *bison-leaf*)
	#:tools  '()
	#:inputs (list musl-stage1 libssp-nonshared-stage1)
	#:build  (gnu-build (conc "bison-" *bison-version*) arch))

(let* ((conf (table '((arch . x86_64))))
       (host (table->proc conf))
       (target host))
  (parameterize ((artifact-dir "./artifacts")
		 (plan-dir     "./plans")
		 (trace-log    #t))
    (build-package! libmpc-stage1 host target)
    (build-package! bison-stage1 host target)))
