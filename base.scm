
(define *prebuilts*
  `((x86_64 . ,(include "prebuilt-x86_64.scm"))
    (aarch64 . ,(include "prebuilt-aarch64.scm"))
    (ppc64le . ,(include "prebuilt-ppc64le.scm"))))

;; for a triple, create the ordinary gcc, ld, as, etc.
;; binaries that are either symlinks or thin wrappers
;; for the actual binaries (i.e. x86_64-linux-musl-gcc, etc.)
(define (native-gcc-toolchain-wrappers triple)
  (let* ((plat (lambda (name) (conc triple "-" name)))
	 (wrap (lambda (name)
		 (interned
		  (string-append "/usr/bin/" name)
		  #o755
		  (string-append
		   "#!/bin/execlineb -s0\n"
		   (plat name) " --sysroot=/ $@\n"))))
	 (ln   (lambda (name)
		 (interned-symlink
		  (string-append "/usr/bin/" name)
		  (string-append "/usr/bin/" (plat name))))))
    (cons*
     (wrap "gcc")
     (wrap "g++")
     (wrap "ld")
     (map ln '("as" "ar" "ranlib" "strip" "readelf" "objcopy")))))

(define native-toolchain
  ;; NOTE: we are presuming that $native-toolchain
  ;; is just wrappers for stuff provided by $cc-toolchain
  (lambda (host)
    (let ((htc ($cc-toolchain host))
	  (ntc ($native-toolchain host)))
      (flatten (cc-toolchain-tools htc)
	       (cc-toolchain-libc htc)
	       (cc-toolchain-tools ntc)
	       (cc-toolchain-libc ntc)))))

;; gcc is a pseudo-package that provides
;; a gcc for build=host plus wrappers that
;; provide /usr/bin/gcc, etc
(define gcc
  (lambda (host)
    (let ((tri ($triple host)))
      (list
       (gcc-for-triple tri)
       (binutils-for-triple tri)
       (native-gcc-toolchain-wrappers tri)))))

(define *default-build-triple*
  (string->symbol (conc *this-machine* "-linux-musl")))

;; gcc+musl-toolchain is a toolchain that produces
;; statically-linked PIE binaries using gcc and musl libc
(define (gcc+musl-toolchain triple #!key
			    (CFLAGS '())
			    (CXXFLAGS '())
			    (LDFLAGS '())
			    (prebuilt #f))
  (let* ((plat   (lambda (bin)
		   (conc triple "-" bin)))
	 ;; n.b.: we configure our gcc toolchain --with-sysroot=<sysroot>
	 ;; so it should automatically use the right one anyway
	 (sysf   (conc "--sysroot=" (triple->sysroot triple)))
	 (lflags (cons sysf '(-static-pie "-Wl,--gc-sections")))
	 (cflags (append lflags '(-fPIE -ffunction-sections -fdata-sections -pipe))))
    (make-cc-toolchain
     tools: (or prebuilt
		(list
		 (gcc-for-triple triple)
		 (binutils-for-triple triple)
		 exportall
		 make
		 execline-tools
		 busybox-core))
     ;; it must be possible to build these with only 'tools:'
     libc: (list musl libssp-nonshared)
     env: (make-cc-env
	   CBUILD:   *default-build-triple* ; FIXME
	   CHOST:    triple
	   CC:       (plat "gcc")
	   CFLAGS:   (append cflags CFLAGS)
	   CXX:      (plat "g++")
	   CXXFLAGS: (append cflags CXXFLAGS)
	   LD:       (plat "ld")
	   LDFLAGS:  (append lflags LDFLAGS)
	   AS:       (plat "as")
	   AR:       (plat "ar")
	   NM:       (plat "nm")
	   ARFLAGS:  "-Dcr"
	   RANLIB:   (plat "ranlib")
	   STRIP:    (plat "strip")
	   READELF:  (plat "readelf")
	   OBJCOPY:  (plat "objcopy")))))

(define (gcc-native-toolchain triple)
  (make-cc-toolchain
   tools: (native-gcc-toolchain-wrappers triple)
   libc:  '()
   env: (make-cc-env
	 CBUILD:   #f
	 CHOST:    #f
	 CC:       "gcc"
	 CFLAGS:   '(-fPIE -static-pie -pipe -O2)
	 CXX:      "g++"
	 CXXFLAGS: '(-fPIE -static-pie -pipe -O2)
	 LD:       "ld"
	 LDFLAGS:  '(-static-pie)
	 AS:       "as"
	 AR:       "ar"
	 NM:       "nm"
	 ARFLAGS:  "-Dcr"
	 RANLIB:   "ranlib"
	 STRIP:    "strip"
	 READELF:  "readelf"
	 OBJCOPY:  "objcopy")))

;; gcc+musl-static-config produces a config that uses
;; gcc and musl libc for static linking
;;
;; 'optflag:' can be used to override the default optimization level,
;; and 'sspflag:' can be used to override the stack protector flag
;;
;; presently, this is the only config/toolchain option,
;; but at some point a clang alternative may be introduced...
(define (gcc+musl-static-config arch
				#!key
				(optflag '-O2)
				(sspflag '-fstack-protector-strong)
				(extra-cflags '())
				(build #f)
				(bootstrap #f)
				(prebuilt #f))
  (let ((badfl  '(-march=native -mtune=native -mcpu=native))
	(triple (string->symbol (conc arch "-linux-musl")))
	(cflags (cons*
		 (or optflag '-Og)
		 (or sspflag '-fno-stack-protector)
		 extra-cflags)))
    ;; don't allow CFLAGS that we know will
    ;; break reproducibility (so far just -march=native and equivalents)
    (let loop ((fl badfl))
      (or (null? fl)
	  (if (memq (car fl) extra-cflags)
	      (error "CFLAGS breaks reproducibility" (car fl))
	      (loop (cdr fl)))))
    (make-config
     arch:         arch
     triple:       triple
     build:        build
     bootstrap:    bootstrap
     cc-toolchain: (gcc+musl-toolchain triple CFLAGS: cflags CXXFLAGS: cflags prebuilt: prebuilt)
     native-cc-toolchain: (gcc-native-toolchain triple))))

;; cc-for-target produces a list of packages
;; that together constitute a C/C++ toolchain
;; for a particular target; if native is supplied
;; and not #f, then symlinks and packages for
;; the a C/C++ toolchain for the build configuration
;; are also included
;;
;; (You will need to supply 'native' when packages
;; need a C/C++ compiler in order to compile tools
;; that are run as part of the overall build process.)
(define (cc-for-target conf #!optional (native #f))
  (let* ((tc    ($cc-toolchain conf))
	 (tools (cc-toolchain-tools tc)))
    (if native
	(cons native-toolchain tools)
	tools)))

(define *musl-url*
  "https://www.musl-libc.org/releases/musl-$version.tar.gz")
(define *musl-version* "1.2.0")
(define *musl-hash* "-DtKaw1hxYkV_hURoMR-00bA9TPByU0RITAnt9ELLls=")

(define (musl-arch-name triple)
  (let ((arch (triple->arch triple)))
    (case arch
      ((ppc64le ppc64) 'powerpc64)
      ((armv7 armv6) 'arm)
      (else arch))))

;; when building a cross-compiler,
;; we need headers for the target system
;;
;; this package is a hack that provides a 'tool'
;; that makes libc headers appear at a particular sysroot
(define musl-headers-for-triple
  (memoize-eq
   (lambda (target-triple)
     (lambda (host)
       (if (eq? ($triple host) target-triple)
	   (error "musl-headers-for-triple native will cause bootstrap loops"))
       (expand-package
	host
	src:   (remote-archive
		(url-translate *musl-url* "musl" *musl-version*)
		*musl-hash*)
	dir:   (string-append "musl-" *musl-version*)
	env:   '()
	label: (conc "musl-headers-" (triple->arch target-triple))
	tools: (list make execline-tools busybox-core)
	inputs: '()
	build: `((make ,(conc "DESTDIR=/out/" (triple->sysroot target-triple))
		   ,(conc "ARCH=" (musl-arch-name target-triple))
		   "prefix=/usr" install-headers)))))))

(define musl
  (cc-package
   "musl" *musl-version*
   *musl-url* *musl-hash*
   no-libc: #t ; compiled as -ffreestanding
   build: (cmd*
	   `(./configure --disable-shared --enable-static
			 --prefix=/usr (CC= ,$CC) (CFLAGS= ,$CFLAGS)
			 --target ,$arch)
	   `(make ,$make-overrides)
	   '(make DESTDIR=/out install))))

(define libssp-nonshared
  (lambda (conf)
    (expand-package
     conf
     label:   (conc "libssp-nonshared-" ($arch conf))
     tools:   (cc-for-target conf)
     inputs:  '()
     dir:     "/src"
     src:     (interned "/src/ssp-nonshared.c" #o644 #<<EOF
			extern void __stack_chk_fail(void);
			void __attribute__((visibility ("hidden"))) __stack_chk_fail_local(void) { __stack_chk_fail(); }
EOF
)
     build: `((if ((,($CC conf) ,@($CFLAGS conf) -c ssp-nonshared.c -o __stack_chk_fail_local.o)))
	      (if ((,($AR conf) -Dcr libssp_nonshared.a __stack_chk_fail_local.o)))
	      (if ((mkdir -p /out/usr/lib)))
	      (cp libssp_nonshared.a /out/usr/lib/libssp_nonshared.a)))))

;; Dependencies necessary to statically link an ordinary C executable
(define libc (list musl libssp-nonshared))

;; exportall(1) is an execline tool for exporting a block of variables
;; (in execline syntax, it works like 'exportall { key val ... } prog ...'
;; so you'd write (exportall ((k v) ...)) in a build script
(define exportall
  (let ((hash "YR0DqwQcQvI7RwjCDNtnoiENnK_fIbitz76HKwdZ0Ms="))
    (cc-package
     "exportall" "0.1"
     (string-append "https://b2cdn.sunfi.sh/pub-cdn/files/" hash)
     hash
     build: (cmd*
	     `(make DESTDIR=/out (CC= ,$CC) (CFLAGS= ,$CFLAGS) install)
	     $strip-cmd))))

(define m4
  (cmmi-package
   "m4" "1.4.18"
   "https://ftp.gnu.org/gnu/$name/$name-$version.tar.gz"
   "_Zto8BBAes0pngDpz96kt5-VLF6oA0wVmLGqAVBdHd0="
   ;; m4 leaves some garbage in /usr/lib
   cleanup: '((if ((rm -rf /out/usr/lib))))))

(define gawk
  (cmmi-package
   "gawk" "5.1.0"
   "https://ftp.gnu.org/gnu/$name/$name-$version.tar.xz"
   "qPUwDbyEw75NjMOALy2nyYWv_LSMprTZEfAYjIos0-c="
   cleanup: '((foreground ((rm -f /out/usr/bin/awk))))))

(define libgmp
  (cmmi-package
   "gmp" "6.2.0"
   "https://gmplib.org/download/gmp/gmp-$version.tar.xz"
   "YQMYgwK95PJL5gS5-l_Iw59tc1O31Kx3X2XFdWm8t6M="
   env: (lambda (conf)
	  ;; gmp's configure script ignores CFLAGS_FOR_BUILD,
	  ;; so we have to shove everything into CC_FOR_BUILD
	  `((CC_FOR_BUILD . ,(cons ($build-CC conf)
				   ($build-CFLAGS conf)))))
   tools: (list m4 native-toolchain)
   extra-configure: '(--with-pic)))

(define libmpfr
  (cmmi-package
   "mpfr" "4.0.2"
   "https://www.mpfr.org/mpfr-current/$name-$version.tar.bz2"
   "wKuAJV_JEeh560Jgqo8Iub6opUuqOKFfQATGEJ2F3ek="
   libs: (list libgmp)))

(define libmpc
  (cmmi-package
   "mpc" "1.1.0"
   "https://ftp.gnu.org/gnu/$name/$name-$version.tar.gz"
   "2lH9nuHFlFtOyT_jc5k4x2CHCtir_YwwX9mg6xoGuTc="
   libs: (list libgmp libmpfr)))

(define bzip2
  (cc-package
   "bzip2" "1.0.8"
   "https://sourceware.org/pub/$name/$name-$version.tar.gz"
   "pZGXjBOF4VYQnwdDp2UYObANElrjShQaRbMDj5yef1A="
   build: (cmd*
	   `(make PREFIX=/out/usr ,$cc-env ,$make-overrides install)
	   $strip-cmd)))

;; wrapper for cmmi-package for skaware,
;; since they all need similar treatment
(define (ska-cmmi-package
	 name version hash
	 #!key
	 (libs '())
	 (extra-configure '()))
  (cmmi-package
   name version
   "https://skarnet.org/software/$name/$name-$version.tar.gz" hash
   libs:     libs
   prepare:  '((if ((sed "-i" -e "/^tryflag.*-fno-stack/d" -e "s/^CFLAGS=.*$/CFLAGS=/g" configure))))
   override-configure: (vargs
			(append
			`(--prefix=/
			  --libdir=/usr/lib
			  --disable-shared --enable-static
			  --target ,$triple
			  (--with-include= ,$sysroot /include)
			  (--with-include= ,$sysroot /usr/include)
			  (--with-lib= ,$sysroot /lib)
			  (--with-lib= ,$sysroot /usr/lib))
			  extra-configure))))

(define skalibs
  (ska-cmmi-package
   "skalibs" "2.9.2.1"
   "-GI9LFINaiNdVFzBFvQxD2880lyqLAC2YpWNdRXUPaE="
   extra-configure: '(--with-sysdep-devurandom=yes)))

(define execline-tools
  (ska-cmmi-package
   "execline" "2.6.0.1"
   "0AwX9jiwZt0b0KiHeuWYvuzZdHlP22cZ0088gDI_iRc="
   libs: (list skalibs)
   extra-configure: `((--with-sysdeps= ,$sysroot /lib/skalibs/sysdeps)
		      --enable-pedantic-posix
		      --enable-static-libc)))
(define byacc
  (cmmi-package
   "byacc" "20200330"
   "https://invisible-mirror.net/archives/$name/$name-$version.tgz"
   "FTAMi_kKoQy3LVJS7qBVMo-rrgG5IlzUK10JGTUaq7c="
   extra-configure: '(--enable-btyacc)))

(define reflex
  (cmmi-package
   "reflex" "20191123"
   "https://invisible-mirror.net/archives/$name/$name-$version.tgz"
   "SsgYKYUlYwedhJWxTvBAO2hdfrAMJ8mNpFjuIveGpSo="
   tools: (list byacc)
   ;; install flex(1) and lex(1) symlinks
   cleanup: '((if ((ln -s reflex /out/usr/bin/lex)))
	      (if ((ln -s reflex++ /out/usr/bin/lex++)))
	      (if ((ln -s reflex /out/usr/bin/flex)))
	      (if ((ln -s reflex++ /out/usr/bin/flex++))))))

(define zlib
  (cmmi-package
   "zlib" "1.2.11"
   "https://zlib.net/$name-$version.tar.gz"
   "K3Q8ig9qMtClPdpflHwS8OkSMrLVItBzEu5beP_szJA="
   ;; not autoconf
   override-configure: '(--static --prefix=/usr --libdir=/lib)))

;; NOTE: the latest version of isl is 0.22.1, but
;; it is not available through any HTTPS mirror that I have found...
;; the gcc infrastructure mirror only includes up to 0.18
(define libisl
  (cmmi-package
   "isl" "0.18"
   "https://gcc.gnu.org/pub/gcc/infrastructure/$name-$version.tar.bz2"
   "bFSNjbp4fE4N5xcaqSGTnNfLPVv7QhnEb2IByFGBZUY="
   libs: (list libgmp)))

;; include a file as literal text at compile time
(define-syntax include-file-text
  (er-macro-transformer
    (lambda (expr rename cmp)
      (let ((arg (cadr expr)))
        (unless (string? arg)
          (syntax-error "include-file-text expects a literal string; got" expr))
        (with-input-from-file arg read-string)))))

(define make
  (cmmi-package
   "make" "4.3"
   "https://ftp.gnu.org/gnu/$name/$name-$version.tar.gz"
   "HaL2VGA5mzktijZa2L_IyOv2OTKTGkf8D-AVI_wvARc="
   ;; can't call exit(3) inside a procedure registered with atexit(3);
    ;; just exit promptly
   prepare: '((if ((sed "-i" -e "s/ exit (MAKE/ _exit (MAKE/g" src/output.c))))))

(define binutils-for-triple
  (memoize-eq
   (lambda (target-triple)
     (cmmi-package
      "binutils" "2.34"
      "https://ftp.gnu.org/gnu/$name/$name-$version.tar.gz"
      "laZMIwGW3GAXUFMyvWCaHwBwgnojWVXGE94gWH164A4="
      tools: (list byacc reflex)
      libs:  (list zlib)
      native-cc: $cc-env/for-build
      prepare: '((if ((sed "-i" -e "s/^SUBDIRS =.*/SUBDIRS =/" binutils/Makefile.in))))
      override-configure: (vargs `(--disable-nls
				   --disable-shared --enable-static
				   --disable-multilib --enable-gold=yes --with-ppl=no
				   --disable-install-libiberty --enable-relro
				   --disable-plugins --enable-deterministic-archives
				   --with-mmap --enable-ld=default
				   --with-system-zlib --enable-64-bit-bfd
				   --disable-install-libbfd
				   --prefix=/usr
				   ;; no libdir, etc. because we discard any
				   ;; libraries and headers produced
				   (--program-prefix= ,target-triple -)
				   (--build= ,$build-triple)
				   (--target= ,target-triple)
				   (--host= ,$triple)
				   (--with-sysroot= ,(triple->sysroot target-triple))))
      cleanup: '((if ((rm -rf /out/usr/include)))
		 (if ((rm -rf /out/include)))
		 (if ((rm -rf /out/usr/lib)))
		 (if ((rm -rf /out/lib))))))))

;; this is a hack that allows us to build a cross-gcc
;; without circular dependencies: install a fake set of
;; object files that define the same symbols that musl does
(define fake-musl-for-triple
  (memoize-eq
    (lambda (target-triple)
      (lambda (host)
	(if (eq? target-triple ($triple host))
	  (error "fake-musl for build machine will cause a bootstrap loop"))
        (let* ((hash "GpFZDYJsPUIYcsfZe-22Qk90kJ9albSjYSf_qTfzuuA=")
               (leaf (remote-archive
                       (conc "https://b2cdn.sunfi.sh/file/pub-cdn/" hash)
                       hash kind: 'tar.zst))
               (version '0.1))
          (expand-package
	   host
	   label: (conc "fakemusl-" version "-" (triple->arch target-triple))
	   src:   leaf
	   dir:   (conc "fakemusl-" version)
	   ;; so, we're hard-coding binutils here rather than
	   ;; getting the toolchain from the host <config>
	   ;; because we actually need an assembler, and
	   ;; clang doesn't come with one
	   tools: (list (binutils-for-triple target-triple)
			make
			execline-tools
			busybox-core)
	   inputs: '()
	   build: (let* ((outdir (filepath-join "/out" (triple->sysroot target-triple))))
		    `((if ((make ,@(k=v* AS: (conc target-triple "-as")
					 AR: (conc target-triple "-ar")) all)))
		      (make PREFIX=/usr ,(conc "DESTDIR=" outdir) install)))))))))

(define (if-native-target? tg yes no)
  (lambda (conf)
    (if (eq? tg ($triple conf))
	yes
	no)))

(define gcc-for-triple
  (memoize-eq
   (lambda (target-triple)
     ;; TODO: make fakelibc work for
     ;; something other than musl
     (let ((fakelibc (lambda (triple)
		       (list (fake-musl-for-triple triple)
			     (musl-headers-for-triple triple)))))
       (cmmi-package
	"gcc" "9.3.0"
	"https://ftp.gnu.org/gnu/$name/$name-$version/$name-$version.tar.gz"
	"Knfr2Y-XW8XSlBKweJ5xdZ50LJhnZeMmxDafNX2LEcM="
	patches: (patch* (include-file-text "patches/gcc/pie-gcc.patch"))
	;; we depend on cc-for-build automatically,
	;; so we only need additional target tools
	;; if target!=build
	tools: (if-native-target?
		target-triple
		(list byacc reflex gawk)
		(cons* byacc reflex gawk
		       (binutils-for-triple target-triple)
		       (fakelibc target-triple)))
	libs: (list libgmp libmpfr libmpc libisl zlib)
	out-of-tree: #t
	extra-cflags: '(-DCROSS_DIRECTORY_STRUCTURE)
	native-cc: $cc-env/for-build
	env:  (lambda (conf)
		 (list ($make-overrides conf)
		       '(gcc_cv_no_pie . no)
		       '(gcc_cv_c_no_pie . no)
		       '(gcc_cv_c_no_fpie . no)))
	prepare: '((if ((find "." -name Makefile.in -exec sed "-i"
			      -e "/^AR = ar/d"        ; please don't hard-code AR
			      -e "/^ARFLAGS = cru/d"  ; please don't hard-code ARFLAGS
			      "{}" ";")))
		   ;; this $(MAKE) command invocation
		   ;; for libraries for the *build* system
		   ;; overrides the wrong variables; the
		   ;; right ones are already in the Makefile(s)
		   ;; for those targets; the configure scripts
		   ;; make sure of that...
		   ;;
		    ;; you can repro this build failure by
		   ;; cross-building a "native" GCC with
		   ;; CFLAGS that are invalid for the *build* system
		   (if ((sed "-i"
			     -e "s/\\$(MAKE) \\$(BASE_FLAGS_TO_PASS) \\$(EXTRA_BUILD_FLAGS)/\\$(MAKE)/g"
			     Makefile.in)))
		   ;; don't pull in GNU tar just to install headers;
		   ;; force the makefile to use busybox cpio instead
		   (if ((sed "-i"
			     -e "s/=install-headers-tar/=install-headers-cpio/g"
			     gcc/config.build))))
	override-configure: (vargs
			     `(--prefix=/usr
			       --exec-prefix=/usr --disable-lto
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
			       --with-mmap --disable-symvers
			       --enable-version-specific-runtime-libs
			       --with-system-zlib
			       "--enable-languages=c,c++"
			       ;; target-specific options:
			       ,@(case (triple->arch target-triple)
				   ((ppc64le ppc64) '(--enable-secureplt --enable-decimal-float=no))
				   (else '()))
			       (--with-sysroot= ,(triple->sysroot target-triple))
			       (--build= ,$build-triple)
			       (--target= ,target-triple)
			       (--host= ,$triple)))
	 cleanup: (if-native-target?
		   target-triple
		   ;; native targets still shouldn't keep /usr/bin/gcc, etc.;
		   ;; we'll install those as symlinks when native-cc is required
		   `((if ((find /out/usr/bin -type f ! -name ,(conc target-triple "*") -delete))))
		   ;; only the native version of gcc should have
		   ;; the python gdb helpers
		   '((if ((elglob dir "/out/usr/share/gcc-*/python")
			  (rm -rf $dir))))))))))

(define (busybox/config config-hash extra-inputs)
  (cc-package
   "busybox" "1.31.1"
   "https://busybox.net/downloads/$name-$version.tar.bz2"
   "JqkfZAknGWBuXj1sPLwftVaH05I5Hb2WusYrYuO-sJk="
   patches:   (patch* (include-file-text "patches/busybox/busybox-bc.patch"))
   extra-src: (list (remote-file #f config-hash "/src/config.head" #o644))
   libs:      extra-inputs
   tools:     (list bzip2)
   use-native-cc: #t
   env:     (list
	     '(KCONFIG_NOTIMESTAMP . 1))
   build:   (cmd*
	     '(mv /src/config.head .config)
	     `(make V=1
		(CROSS_COMPILE= ,$cross-compile)
		(CONFIG_SYSROOT= ,$sysroot)
		(CONFIG_EXTRA_CFLAGS= ,$CFLAGS)
		,$cc-env/for-kbuild
		busybox)
              '(make V=1 busybox.links)
	      '(install -D -m "755" busybox /out/bin/busybox)
	      '(mkdir -p /out/usr/bin /out/sbin /out/usr/sbin)
	      '((redirfd -r 0 busybox.links)
		(forstdin -o 0 link)
		(importas "-i" -u link link)
		(ln -s /bin/busybox "/out/${link}")))))

;; busybox-core is just enough busybox to build packages;
;; it doesn't include system utilities that would require
;; linux headers
(define busybox-core
  (busybox/config "OE8osvZRzHk6NO3aMhnF6uyZUwlpYZtOz8LF8bR2V6k=" '()))

(define *linux-major* 5.4)
(define *linux-patch* 39)

(define *linux-source*
  (list
    (remote-archive
     (conc "https://cdn.kernel.org/pub/linux/kernel/v5.x/linux-" *linux-major* ".tar.xz")
     "SUt0rAz8S3yXkXuSN8sG6lm4sW7Bvssxg_oAKuNjqzs=")
    (remote-file
     (conc "https://cdn.kernel.org/pub/linux/kernel/v5.x/patch-" *linux-major* "." *linux-patch* ".xz")
     "1L9Wy6dDCSnQFwBJcKT4QMJtYq0tYcDS3wSeMDD-07o="
     "/src/linux.patch"
     #o644)))

(define (linux-arch-name arch)
  (case arch
    ((x86_64)        'x86_64)
    ((x86)           'i386)
    ((ppc64 ppc64le) 'powerpc)
    ((aarch64)       'arm64)
    ((armv7)         'arm)
    (else (error "unrecognized arch" arch))))

(define (linux-arch-dir-name arch)
  (let ((aname (linux-arch-name arch)))
    (case aname
      ((i386 x86_64) 'x86)
      (else aname))))

(define linux-headers
  (lambda (conf)
    (expand-package
     conf
     src:    *linux-source*
     dir:    (conc "linux-" *linux-major*)
     label:  (conc "linux-headers-" *linux-major* "." *linux-patch* "-" ($arch conf))
     tools:  (list xz-utils native-toolchain)
     inputs: '()
     build:  `((if ((pipeline ((xzcat /src/linux.patch)))
		    (patch -p1)))
	       (if ((make ,(conc 'ARCH= (linux-arch-name ($arch conf)))
		      ,@(kvargs ($cc-env/for-kbuild conf))
		      headers)))
	       ;; headers_install uses rsync, which is a
	       ;; silly large dependency to pull in
	       ;; at this stage...
	       (if ((cp -r usr /out)))
	       (find /out -type f ! -name "*.h" -delete)))))

(define (installkernel* script)
  (interned "/sbin/installkernel" #o755
            (cond
              ((string? script)
               script)
              ((pair? script)
               (with-output-to-string
                 (lambda () (write-exexpr script))))
              (else (error "don't know how to write as a script:" script)))))

;; based on reading arch/*/boot/install.sh:
;;
;; $1 = version
;; $2 = image file
;; $3 = map file
;; $4 = destination (we already know this...)
;; (and sometimes there are trailing arguments)
(define *default-installkernel* #<<EOF
#!/bin/execlineb -s3
if { mkdir -p /out/boot }
if {
  redirfd -r 0 $2
  redirfd -x 1 /out/boot/vmlinuz
  cat
}
cp $3 /out/boot/System.map

EOF
)

;; yypush_buffer_state() and yypop_buffer_state()
;; are GNU extensions
(define portable-lexer-patch #<<EOF
--- a/scripts/dtc/dtc-lexer.l
+++ b/scripts/dtc/dtc-lexer.l
@@ -277,15 +277,21 @@

 %%

+#define __MAX_INCLUDES 32
+static int __n_includes = 0;
+static void *__fstack[__MAX_INCLUDES];
+
 static void push_input_file(const char *filename)
 {
 	assert(filename);
+        assert(__n_includes < __MAX_INCLUDES);

 	srcfile_push(filename);
+        __fstack[__n_includes++] = YY_CURRENT_BUFFER;

 	yyin = current_srcfile->f;

-	yypush_buffer_state(yy_create_buffer(yyin, YY_BUF_SIZE));
+	yy_switch_to_buffer(yy_create_buffer(yyin, YY_BUF_SIZE));
 }


@@ -293,8 +299,8 @@
 {
 	if (srcfile_pop() == 0)
 		return false;
-
-	yypop_buffer_state();
+        yy_delete_buffer(YY_CURRENT_BUFFER);
+        yy_switch_to_buffer(__fstack[--__n_includes]);
 	yyin = current_srcfile->f;

 	return true;
EOF
)

;; common fixes to scripts/dtc between linux and uboot;
;; the dtc yacc+lex files do not play nicely with BSD yacc+lex
(define (fix-dtc-script #!key
                        (fix-lex-options #f)  ;; scripts/kconfig/<foo>.l
                        (fix-yacc-cmdline #f)) ;; scripts/Makefile.host or scripts/Makefile.lib
  (unless (and fix-lex-options fix-yacc-cmdline)
    (error "fix-dtc-script is missing required keyword arguments"))
  `((if ((sed "-i" -e "/^%option/s/ full / /" ,fix-lex-options)))
    (if ((sed "-i" -e "3a override YACC:=$(YACC) -L"
              scripts/dtc/Makefile)))
    (if ((sed "-i"
              -e "/^extern.*yyerror(/a #define YYERROR_CALL(msg) yyerror(msg)" scripts/dtc/dtc-parser.y)))
    ;; byacc: use -H <file> instead of --defines=<file>
    (if ((sed "-i" -e "/cmd_bison/s/--defines=/-H /" ,fix-yacc-cmdline)))))

;; linux/config takes a name and configuration hash
;; and produces a kernel package named "linux-<version>-<name>"
;; using the given configuration file
;;
;; the kernel configuration is determined by running
;;   'make KCONFIG_ALLCONFIG=<config> allnoconfig'
;; so you can simply include a config with only the
;; explicit configuration variables you're interested in
;; and let Kbuild figure out the rest
;;
;; this build option only supports configs with CONFIG_MODULES=n
;; (i.e. self-contained kernels without loadable modules)
(define (linux/config-static name config-hash #!key
                             (install *default-installkernel*)
                             (dtb     #f))
  (let ((config  (remote-file #f config-hash "/src/config" #o644))
        (install (installkernel* install))
        (patches (patch* portable-lexer-patch)))
    (lambda (conf)
      (expand-package
       conf
       src:   (append (list install config) *linux-source* patches)
       dir:   (conc "linux-" *linux-major*)
       label: (conc "linux-" *linux-major* "." *linux-patch* "-" name)
       tools: (cons*
	       perl xz-utils reflex byacc libelf zlib linux-headers
	       (cc-for-target conf #t))
       inputs: '()
       env:   `((KCONFIG_NOTIMESTAMP . 1)
		(KBUILD_BUILD_TIMESTAMP . "@0")
		(KBUILD_BUILD_USER . distill)
		(KBUILD_BUILD_HOST . distill)
		(CROSS_COMPILE . ,($cross-compile conf)))
       patches: patches
       build: (let ((make-args (append
				(kvargs ($cc-env/for-kbuild conf))
				(k=v*
				 YACC: 'yacc ;; not bison -y
				 ARCH: (linux-arch-dir-name ($arch conf))
				 HOST_LIBELF_LIBS: '(-lelf -lz)))))
		`((if ((pipeline ((xzcat /src/linux.patch)))
		       (patch -p1)))
		  ,@(fix-dtc-script
		     fix-lex-options: 'scripts/kconfig/lexer.l
		     fix-yacc-cmdline: 'scripts/Makefile.host)
		  ;; libelf is built with zlib, so -lelf should imply -lz
		  (if ((find "." -type f -name "Make*"
			     -exec sed "-i" -e
			     "s/-lelf/-lelf -lz/g" "{}" ";")))
		  (if ((make
			   V=1 KCONFIG_ALLCONFIG=/src/config
			   ,@make-args
			   allnoconfig)))
		  (if ((make V=1 ,@make-args)))
		  ,@(if dtb
			`((if ((install -D -m "644" -t /out/boot ,dtb))))
			'())
		  (make V=1 ,@make-args install)))))))

(define linux-virt-x86_64
  (linux/config-static "virt-x86_64" "FTMQoxE4ClKOWLDdcDVzWt8UuizXfMmR4duX8Z-5qlY="))

;; libelf is *just* libelf.a and headers;
;; it does not include the rest of elfutils
(define libelf
  (let (($cflags (lambda (conf)
		   (cons* '-D_GNU_SOURCE '-DHAVE_CONFIG_H '-I. '-I.. '-I../lib ($CFLAGS conf))))
	(config  (remote-file
                     #f "ralu1MH7h3xuq7QdjYTneOmZLEoU1RygVrTAWaKl2YY=" "/src/config.h" #o644)))
    (cc-package
     "elfutils" "0.178"
     "https://sourceware.org/$name/ftp/$version/$name-$version.tar.bz2"
     "ibDvVn8CMIhlIQAZGAsl7Yf13fm42qO7NJDctqLd2Hc="
     libs: (list zlib)
     extra-src: (list config)
     ;; the elfutils configure script is a basket case,
     ;; and so are the headers, so we're just massaging
     ;; the source and then building it manually.
     ;; (the included config.h would be generated by ./configure
     ;; if the script could actually run correctly...)

     build: (cmd*
	     `(cp /src/config.h config.h)
	     '(find lib/ libelf/ -type f -name "*.[ch]"
		    -exec sed "-i"
		    -e "/#include <libintl.h>/d"
		    -e "/#include.*cdefs.h>/d"
		    "{}" ";")
	     '(find lib/ -type f -name "*.h"
		    -exec sed "-i" -e "s/<error.h>/<err.h>/g" "{}" ";")
	     `((cd libelf/)
	       (elglob -s csrc "*.c")
	       (,$CC ,$cflags -c $csrc))
	     `((cd libelf/)
	       (elglob -s objs "*.o")
	       (,$AR ,$ARFLAGS libelf.a $objs))
	     '(mkdir -p /out/usr/include /out/usr/lib)
	     '(cp libelf/gelf.h /out/usr/include/gelf.h)
	     '(cp libelf/libelf.h /out/usr/include/libelf.h)
	     '(cp libelf/libelf.a /out/usr/lib/libelf.a)))))

;; mke2fs -d <dir> does not have a way
;; to bypass timestamps and override uid/gid,
;; so this hacks in a way to at least ensure
;; that those fields get zeroed...
(define mke2fs-repro-patch #<<EOF
--- a/misc/create_inode.c
+++ b/misc/create_inode.c
@@ -17,6 +17,7 @@
 #include <time.h>
 #include <sys/stat.h>
 #include <sys/types.h>
+#include <stdlib.h>
 #include <unistd.h>
 #include <limits.h> /* for PATH_MAX */
 #include <dirent.h> /* for scandir() and alphasort() */
@@ -128,6 +129,16 @@
 	inode.i_atime = st->st_atime;
 	inode.i_mtime = st->st_mtime;
 	inode.i_ctime = st->st_ctime;
+
+        if (getenv("MKE2FS_DETERMINISTIC")) {
+                inode.i_uid = 0;
+                ext2fs_set_i_uid_high(inode, 0);
+                inode.i_gid = 0;
+                ext2fs_set_i_gid_high(inode, 0);
+                inode.i_atime = 0;
+                inode.i_mtime = 0;
+                inode.i_ctime = 0;
+        }

 	retval = ext2fs_write_inode(fs, ino, &inode);
 	if (retval)
EOF
)

(define e2fsprogs
  ;; e2fsprogs is unusual and uses BUILD_CC, BUILD_CFLAGS, etc.
  ;; in order to indicate which CC to use for building tools
  (let (($buildcc-env  (lambda (conf)
			 (cc-env/build
			  conf
			  (lambda (kw)
			    (string->keyword
			     (string-append
			      "BUILD_"
			      (keyword->string kw))))))))
    (cmmi-package
     "e2fsprogs" "1.45.5"
     "https://kernel.org/pub/linux/kernel/people/tytso/$name/v$version/$name-$version.tar.xz"
     "w7R6x_QX6QpTEtnNihjwlHLBBtfo-r9RrWVjt9Nc818="
     patches: (patch* mke2fs-repro-patch)
     native-cc: $buildcc-env
     libs: (list linux-headers)
     ;; atypical:
     ;; e2fsprogs wants BUILD_CC, etc. to be specified
     ;; as configure script arguments
     extra-configure: (vargs `(--enable-symlink-install
			       --enable-libuuid
			       --enable-libblkid
			       --disable-uuidd
			       --disable-fsck
			       ,$buildcc-env))
     override-install: '("MKDIR_P=install -d" DESTDIR=/out install install-libs))))

(define perl
  ;; the perl configure script desperately
  ;; wants to put date(1) output into the build output,
  ;; so just overwrite the date(1) executable
  (let ((samedate
	 (interned "/bin/samedate" #o755
		   (lambda ()
		     (write-exexpr
                      '((echo "Fri Apr 3 20:09:47 UTC 2020"))
                      shebang: "#!/bin/execlineb -s0")))))
    (cc-package
     "perl" "5.30.2"
     "https://www.cpan.org/src/5.0/$name-$version.tar.gz"
     "1LHYN7a4aIRWLowcJRJWNNTMSLl5btVOydsEyFCL2Yo="
     tools: (list samedate)
     libs:  (list bzip2 zlib)
     env:   (lambda (conf)
	      `((BUILD_ZLIB . 0)
		(BUILD_BZIP2 . 0)
		(BZIP2_LIB . ,(filepath-join ($sysroot conf) "/usr/lib"))
		(BZIP2_INCLUDE . ,(filepath-join ($sysroot conf) "/usr/include"))))
     build: (cmd*
	     '(ln -sf /bin/samedate /bin/date)
	     `(./Configure -des (-Dcc= ,$CC)
			   (-Dccflags= ,$CFLAGS)
			   (-Dar= ,$AR)
			   (-Darflags= ,$ARFLAGS)
			   (-Dld= ,$LD)
			   (-Dldflags= ,$LDFLAGS)
                           (-Doptimize= -Os) ;; TODO: pull this out of $CFLAGS
			   (-Dnm= ,$NM)
			   (-Dranlib= ,$RANLIB)
			   (-Dsysroot= ,$sysroot)
			   ;; force reproducible uname
			   -A "define:osname=linux"
			   -A "define:osvers=5.4.x"
			   -Dmyuname=distill-builder -Dcf_by=distill-builder
			   -Dprefix=/usr -Dprivlib=/usr/share/perl5/core_perl
			   -Darchlib=/usr/lib/perl5/core_perl -Dvendorprefix=/usr
			   -Dvendorlib=/usr/share/perl5/vendor_perl -Dvendorarch=/usr/lib/perl5/vendor_perl
			   -Duselargefiles -Dusethreads -Duseshrplib=false -Dd_semctl_semun
			   -Dman1dir=/usr/share/man/man1 -Dman3dir=/usr/share/man/man3
			   -Dinstallman1dir=/usr/share/man/man1 -Dinstallman3dir=/usr/share/man/man3
			   -Dman1ext=1 -Dman3ext=3pm -Ud_csh -Uusedl -Dusenm
			   -Dusemallocwrap)
	     `(make ,$make-overrides)
	     '(make DESTDIR=/out install)
	     '(rm -rf /out/usr/share/man)
	     '(find /out -name ".*" -delete)))))

;; uboot/config accepts 4 arguments:
;;  - name: a suffix added to the package name
;;    (the package will be named "uboot-<version>-<name>"
;;  - hash: the hash of the .config
;;  - env:  a list of key=value strings that populate
;;    the default environment for the bootloader
;;  - bootcmd: the default kernel boot command for u-boot (i.e. booti, etc.)
(define (uboot/config name hash env bootcmd)
  (let ((envfile (interned
		  "/src/uboot-env"
		  #o644
		  (lines env)))
	(dotconf (remote-file
		  #f hash "/src/uboot-config" #o644)))
    (cc-package
     "u-boot" "2020.04"
     "https://ftp.denx.de/pub/$name/$name-$version.tar.bz2"
     "lDUHMuGJodiUbDG80Pq2uKUbMclFpohyWPjIwSKddtE="
     no-libc:   #t
     use-native-cc: #t
     patches:   (patch* portable-lexer-patch)
     extra-src: (list envfile dotconf)
     tools:     (list byacc reflex)
     build: (lambda (conf)
	      (let ((make-args (vargs `("YACC=yacc -d"
					(CONFIG_BOOTCOMMAND= ,bootcmd)
					(CROSS_COMPILE= ,$cross-compile)
					,$cc-env/for-kbuild))))
		`((importas -D 0 epoch SOURCE_DATE_EPOCH)
		  (backtick SOURCE_DATE_EPOCH
			    ((pipeline ((echo -n $epoch)))
			     (sed -e "s/@//")))
		  (if ((cp /src/uboot-config .config)))
		  ,@(fix-dtc-script
		     fix-lex-options: 'scripts/kconfig/zconf.l
		     fix-yacc-cmdline: 'scripts/Makefile.lib)
		  ;; busybox xxd doesn't support '-i'; emulate it with hexdump
		  (if ((sed "-i"
			    -e "s/xxd -i/hexdump -v -e '\\/1 \"0x%X, \"'/g"
			    -e "s/echo \", 0x00\"/echo \" 0x00\"/g" Makefile)))
		  ;; we're using yacc -d, so the zconf.tab.c needs to #include the generated definitions
		  (if ((sed "-i"
			    -e "/^#include \"/a #include \"zconf.tab.h\"" scripts/kconfig/zconf.y)))
		  (if ((make V=1 ,@(make-args conf))))
		  (install -D -m 644 -t /out/boot u-boot.bin)))))))

(define xz-utils
  (cmmi-package
   "xz" "5.2.5"
   "https://tukaani.org/$name/$name-$version.tar.xz"
   "-jw6foy_zDVKZ8pXQV2O2i1_6Zcs3efYVg31jflUtcQ="))

(define lz4
  (cc-package
  "lz4" "1.9.2"
  "https://github.com/lz4/$name/archive/v$version.tar.gz"
  "uwHhgT74Tk7ds0TQeFZTodoI1_5IZsRnVRNNHi7ywlc="
  build: (cmd*
	  `(make DESTDIR=/out PREFIX=/usr ,$cc-env ,$make-overrides install)
	  $strip-cmd)))

(define zstd
  (cc-package
   "zstd" "1.4.4"
   "https://github.com/facebook/$name/archive/v$version.tar.gz"
   "PKNr93GxvtI1hA4Oia-Ut7HNNGjAcxlvfSr3TYdpdX4="
   build: (let ((makeflags '(HAVE_PTHREAD=1
			     HAVE_ZLIB=0
			     HAVE_LZMA=0
			     HAVE_LZ4=0
			     ZSTD_LEGACY_SUPPORT=0
			     ZSTD_LIB_DEPRECATED=0)))
	    (cmd*
	     `((cd lib/)
	       (make PREFIX=/usr DESTDIR=/out ,$cc-env ,$make-overrides ,@makeflags install-static install-includes))
	     `((cd programs/)
	       (make ,$cc-env ,$make-overrides ,@makeflags zstd))
	     '(install -D -m "755" programs/zstd /out/usr/bin/zstd)))))

(define squashfs-tools
  (let ((ver "4.4"))
    (cc-package
     "squashfs-tools" ver
     "https://github.com/plougher/$name/archive/$version.tar.gz"
     "o-ja9XdUjDj8KcrNOfKi7jQ1z37f7dtf3YUFgqRTIuo="
     ;; non-standard directory:
     dir:   (string-append "squashfs-tools-" ver "/squashfs-tools")
     env:   (csubst (lambda (subst) (list (subst $cc-env))))
     libs:  (list zstd lz4 xz-utils zlib)
     build: (cmd*
	     `(make XZ_SUPPORT=1 LZO_SUPPORT=0
		    LZ4_SUPPORT=1 ZSTD_SUPPORT=1 XATTR_SUPPORT=0 ,$make-overrides)
	     '(mkdir -p /out/usr/bin)
	     '(cp mksquashfs unsquashfs /out/usr/bin)
	     $strip-cmd))))

(define libressl
  (cmmi-package
   "libressl" "3.1.0"
   "https://ftp.openbsd.org/pub/OpenBSD/LibreSSL/$name-$version.tar.gz"
   "cHEFVDGIUxRBRs64A9_ap1jzVOkWv3VF-cyNGYpJvMM="
   cleanup: '((if ((ln -s openssl /out/usr/bin/libressl))))))

(define libarchive
  (cmmi-package
   "libarchive" "3.4.2"
   "https://github.com/libarchive/$name/releases/download/v$version/$name-$version.tar.gz"
   "t3aJd9_ChlLWsDKodSqu7kjPB_4UaCrSb29EqmGq0T8="
   libs: (list bzip2 zlib xz-utils lz4 libressl zstd)
   extra-configure: '(--without-xml2 --without-acl --without-attr --without-expat)))

(define libmnl
  (cmmi-package
   "libmnl" "1.0.4"
   "https://netfilter.org/projects/$name/files/$name-$version.tar.bz2"
   "kUOLXuIdscWD_5WHBvAIuytyuy-gGm_LXen3TWodgNs="
   libs: (list linux-headers)))

(define libnftnl
  (cmmi-package
   "libnftnl" "1.1.6"
   "https://netfilter.org/projects/$name/files/$name-$version.tar.bz2"
   "I_fTUHv8QpyNI5OlM_INMUDXHWsI6V-WzHKhCcfEenA="
   libs: (list linux-headers libmnl)
   extra-configure: '(LIBMNL_CFLAGS=-lmnl
		      LIBMNL_LIBS=-lmnl)))

(define iptables
  (cmmi-package
   "iptables" "1.8.4"
   "https://netfilter.org/projects/$name/files/$name-$version.tar.bz2"
   "hBWYiIU2PYebYoMF6_n_anAFXGfruGBmAXU94ge9DQo="
   libs: (list linux-headers libnftnl libmnl)
   extra-cflags: '(-D_GNU_SOURCE)
   extra-configure: '(libmnl_CFLAGS=-lmnl
		      libmnl_LIBS=-lmnl
		      libnftnl_CFLAGS=-lnftnl
		      libnftnl_LIBS=-lnftnl)))

(define iproute2
  (let ((tools (lambda (conf)
		 (list
		  byacc reflex
		  (interned
		   "/src/config.mk"
		   #o644
		   (lines
		    (append
		     '("YACC=yacc")
		     (splat
		      ($cc-env conf)
		      CC: LDFLAGS: AR:)
		     (list
		      "TC_CONFIG_IPSET:=y"
		      "TC_CONFIG_NO_XT:=y"
		      "HAVE_MNL:=y"
		      "CFLAGS += -DHAVE_ELF -DHAVE_SETNS -DHAVE_LIBMNL"
		      "LDLIBS += -lelf -lmnl -lz"
		      "%.o: %.c"
		      "\t$(CC) $(CFLAGS) -c -o $@ $<"))))))))
	(cc-package
	 "iproute2" "5.6.0"
	 "https://kernel.org/pub/linux/utils/net/$name/$name-$version.tar.xz"
	 "bziQSr_HXdEGLJPNH0jdUOKiuehV0HY1KI95UkV4cC0="
	 patches: (list
		   (remote-file
		    "https://git.alpinelinux.org/aports/plain/main/iproute2/musl-fixes.patch"
		    "K4srcIY08guTgXv7DeGR6InxsXUKFV76vmeLao7Y0Cw="
		    "/src/musl-fixes.patch"
		    #o644)
		   (remote-file
		    "https://git.alpinelinux.org/aports/plain/main/iproute2/fix-install-errors.patch"
		    "jUzhNv5c3_lyQZ6omNKQaBvZNbpHZVkyeMuG15uq1sA="
		    "/src/fix-install-errors.patch"
		    #o644))
	 tools: tools
	 libs:  (list linux-headers iptables libmnl libelf zlib)
	 build: (cmd*
		 '(cp /src/config.mk config.mk)
                 '(sed "-i" -e "/^SUBDIRS/s: netem::" Makefile)
                 `(make (CCOPTS= ,$CFLAGS) SHARED_LIBS=n PREFIX=/usr all)
                 '(make SHARED_LIBS=n DESTDIR=/out PREFIX=/usr install)
                 '(rm -rf /out/usr/share/bash-completion)
                 '(rm -rf /out/var)
                 '(rm -rf /out/usr/share/man)
		 $strip-cmd))))

(define s6
  (ska-cmmi-package
   "s6" "2.9.1.0"
   "-YjvL_kpeegF4FFOlDizjsxkC8gSqPftHDsII5sDmEc="
   libs: (list skalibs execline-tools)
   extra-configure: `((--with-sysdeps= ,$sysroot /lib/skalibs/sysdeps)
		      --enable-static-libc)))

(define s6-rc
  (ska-cmmi-package
   "s6-rc" "0.5.1.2"
   "y8awbxd6B7btH4qmlyx2FWwhdlysT4n19Twc-x0lotc="
   libs: (list s6 skalibs execline-tools)
   extra-configure: `((--with-sysdeps= ,$sysroot /lib/skalibs/sysdeps)
		      --enable-static-libc)))

;; hard(8) command; an alternative to busybox halt(8)/reboot(8)/poweroff(8)
(define hard
  (let ((hash "aVGnVsRk_al4CfeliyuIZsyj7LBG-GphmUM-BgHad7E="))
    (cc-package
     "hard" "0.1"
     (string-append "https://b2cdn.sunfi.sh/file/pub-cdn/" hash)
     hash
     build: (cmd*
	     `(make (CC= ,$CC) (CFLAGS= ,$CFLAGS) (LDFLAGS= ,$LDFLAGS) DESTDIR=/out install)
	     $strip-cmd))))

(define busybox-full
  (busybox/config
    "kHCLlhEuZrIcR3vjYENuNyI1a0eGB1B6APiyWjvkvok="
    (list linux-headers)))

;; keep everything down here at the bottom of the file;
;; we need all the base packages to be declared in order
;; for gcc+musl-static-config not to reference unbound variables

;; default-config produces the default config for 'arch'
;; which should be one of '(x86_64 aarch64 ppc64le)
(define (default-config arch)
  (or (memq arch '(x86_64 aarch64 ppc64le))
      (info "WARNING: un-tested architecture" arch))
  (if (eq? arch *this-machine*)
      (force default-build-config)
      (gcc+musl-static-config arch
			      optflag: '-Os
			      sspflag: '-fstack-protector-strong
			      build:   (force default-build-config))))

;; set package#build-config to
;; the default config for this machine
(define default-build-config
  (delay
    (let* ((optflag '-Os)
	   (sspflag '-fstack-protector-strong)
	   (config* (lambda args
		      (apply gcc+musl-static-config
			     *this-machine*
			     optflag: optflag
			     sspflag: sspflag
			     args)))
	   (stage0  (config*
		     prebuilt: (map cdr (cdr (assq *this-machine* *prebuilts*)))
		     bootstrap: #f
		     build: #f))
	   (stage1  (config*
		     bootstrap: stage0
		     build: #f)))
      (config* build: #f bootstrap: stage1))))
