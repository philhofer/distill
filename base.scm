
(define *prebuilts*
  `((x86_64 . ,(include "prebuilt-x86_64.scm"))
    (aarch64 . ,(include "prebuilt-aarch64.scm"))
    (ppc64le . ,(include "prebuilt-ppc64le.scm"))))

;; some stuff is simply stored here by its content hash
(define (cdn-url c-hash)
  (string-append
   "https://b2cdn.sunfi.sh/file/pub-cdn/"
   c-hash))

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
(define *musl-version* "1.2.1")
(define *musl-hash* "sw1MSXLr0ImjAIyeZvc3FLJiB0yFLh80eu8BGuQ12WM=")

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
     (package-template
      src:   (remote-archive
	            (url-translate *musl-url* "musl" *musl-version*)
	            *musl-hash*)
      dir:   (string-append "musl-" *musl-version*)
      env:   '()
      label: (conc "musl-headers-" (triple->arch target-triple))
      tools: (list make execline-tools busybox-core)
      inputs: '()
      build: `(make ,(conc "DESTDIR=/out/" (triple->sysroot target-triple))
		            ,(conc "ARCH=" (musl-arch-name target-triple))
		            "prefix=/usr" install-headers)))))

(define musl
  (cc-package
   "musl" *musl-version*
   *musl-url* *musl-hash*
   no-libc: #t ; compiled as -ffreestanding
   build: (elif*
	         `(./configure --disable-shared --enable-static
			                   --prefix=/usr ,(el= 'CC= $CC) ,(el= 'CFLAGS= $CFLAGS)
			                   --target ,$arch)
	         `(make ,$make-overrides)
	         '(make DESTDIR=/out install))))

(define libssp-nonshared
  (package-template
   label:   "libssp-nonshared"
   cross:   (list $cc-tools)
   tools:   '() ; just cc-tools
   inputs:  '()
   dir:     "/src"
   src:     (bind "patches/ssp-nonshared/ssp-nonshared.c" "/src/ssp-nonshared.c")
   build: (elif*
	         `(,$CC ,$CFLAGS -c ssp-nonshared.c -o __stack_chk_fail_local.o)
	         `(,$AR -Dcr libssp_nonshared.a __stack_chk_fail_local.o)
	         '(mkdir -p /out/usr/lib)
	         '(cp libssp_nonshared.a /out/usr/lib/libssp_nonshared.a))))

;; exportall(1) is an execline tool for exporting a block of variables
;; (in execline syntax, it works like 'exportall { key val ... } prog ...'
;; so you'd write (exportall ((k v) ...)) in a build script
(define exportall
  (let ((hash "YR0DqwQcQvI7RwjCDNtnoiENnK_fIbitz76HKwdZ0Ms="))
    (cc-package
     "exportall" "0.1"
     (cdn-url hash)
     hash
     build: (elif*
	           `(make DESTDIR=/out ,(el= 'CC= $CC) ,(el= 'CFLAGS= $CFLAGS) install)
	           (list $strip-cmd)))))

(define m4
  (cmmi-package
   "m4" "1.4.18"
   "https://ftp.gnu.org/gnu/$name/$name-$version.tar.gz"
   "_Zto8BBAes0pngDpz96kt5-VLF6oA0wVmLGqAVBdHd0="
   ;; m4 leaves some garbage in /usr/lib
   cleanup: '(rm -rf /out/usr/lib)))

(define gawk
  (cmmi-package
   "gawk" "5.1.0"
   "https://ftp.gnu.org/gnu/$name/$name-$version.tar.xz"
   "qPUwDbyEw75NjMOALy2nyYWv_LSMprTZEfAYjIos0-c="
   cleanup: '(rm -f /out/usr/bin/awk)))

(define libgmp
  (cmmi-package
   "gmp" "6.2.0"
   "https://gmplib.org/download/gmp/gmp-$version.tar.xz"
   "YQMYgwK95PJL5gS5-l_Iw59tc1O31Kx3X2XFdWm8t6M="
   ;; gmp's configure script ignores CFLAGS_FOR_BUILD,
   ;; so we have to shove everything into CC_FOR_BUILD
   env: `((CC_FOR_BUILD ,$build-CC ,$build-CFLAGS))
   tools: (list m4 native-toolchain)
   extra-configure: '(--with-pic)))

(define libmpfr
  (cmmi-package
   "mpfr" "4.1.0"
   "https://ftp.gnu.org/gnu/$name/$name-$version.tar.bz2"
   "72Rb7WvnPA9a0c4r-UdkHoHqzubqlKae8x40RrGulfM="
   libs: (list libgmp)))

(define libmpc
  (cmmi-package
   "mpc" "1.2.0"
   "https://ftp.gnu.org/gnu/$name/$name-$version.tar.gz"
   "3MXNkST2WkPuntknubd9CJdkEuuWsVXJITtFP4MBHYs="
   libs: (list libgmp libmpfr)))

(define %bzip2
  (cc-package
   "bzip2" "1.0.8"
   "https://sourceware.org/pub/$name/$name-$version.tar.gz"
   "pZGXjBOF4VYQnwdDp2UYObANElrjShQaRbMDj5yef1A="
   build: (elif*
	         `(make PREFIX=/out/usr ,$cc-env ,$make-overrides install)
	         (list $strip-cmd))))

(define bzip2  (binaries %bzip2))
(define libbz2 (libs %bzip2))

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
   prepare:  '(sed "-i" -e "/^tryflag.*-fno-stack/d" -e "s/^CFLAGS=.*$/CFLAGS=/g" configure)
   override-configure: `(--prefix=/
			                   --libdir=/usr/lib
			                   --disable-shared --enable-static
			                   --target ,$triple
			                   ,(elconc '--with-include= $sysroot '/include)
			                   ,(elconc '--with-include= $sysroot '/usr/include)
			                   ,(elconc '--with-lib= $sysroot '/lib)
			                   ,(elconc '--with-lib= $sysroot '/usr/lib)
			                   ,@extra-configure)))

(define skalibs
  (ska-cmmi-package
   "skalibs" "2.9.2.1"
   "-GI9LFINaiNdVFzBFvQxD2880lyqLAC2YpWNdRXUPaE="
   extra-configure: '(--with-sysdep-devurandom=yes)))

(define libexecline+tools
  (ska-cmmi-package
   "execline" "2.6.1.1"
   "uT3m68KBynOnxZHVGfu3ZjXV1ODRVSNNNfF7HrJEqEI="
   libs: (list skalibs)
   extra-configure: `(,(elconc '--with-sysdeps= $sysroot '/lib/skalibs/sysdeps)
		                  --enable-pedantic-posix
		                  --enable-static-libc)))

(define execline-tools (binaries libexecline+tools))
(define libexecline (libs libexecline+tools))

(define byacc
  (cmmi-package
   "byacc" "20200910"
   "https://invisible-mirror.net/archives/$name/$name-$version.tgz"
   "W8Cw05mawBuSJTA0VGhVlFW0dmI0pR6dcOUxqeUnZLI="
   extra-configure: '(--enable-btyacc)))

(define reflex
  (cmmi-package
   "reflex" "20200715"
   "https://invisible-mirror.net/archives/$name/$name-$version.tgz"
   "xopKaaL7wprugSHJBnTvdmfSwb7qZfG-0_5KhtYnx1Q="
   tools: (list byacc)
   ;; install flex(1) and lex(1) symlinks
   cleanup: (elif*
	           '(ln -s reflex /out/usr/bin/lex)
	           '(ln -s reflex++ /out/usr/bin/lex++)
	           '(ln -s reflex /out/usr/bin/flex)
	           '(ln -s reflex++ /out/usr/bin/flex++))))

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

(define make
  (cmmi-package
   "make" "4.3"
   "https://ftp.gnu.org/gnu/$name/$name-$version.tar.gz"
   "HaL2VGA5mzktijZa2L_IyOv2OTKTGkf8D-AVI_wvARc="
   ;; can't call exit(3) inside a procedure registered with atexit(3);
    ;; just exit promptly
   prepare: '(sed "-i" -e "s/ exit (MAKE/ _exit (MAKE/g" src/output.c)))

(define binutils-for-triple
  (memoize-eq
   (lambda (target-triple)
     (cmmi-package
      "binutils" "2.35"
      "https://ftp.gnu.org/gnu/$name/$name-$version.tar.gz"
      "T4QnOCw-pixGZ40MF5PY_yQuLVmiUB_PX065is3HXgM="
      tools: (list byacc reflex)
      libs:  (list zlib)
      native-cc: $cc-env/for-build
      prepare: '(sed "-i" -e "s/^SUBDIRS =.*/SUBDIRS =/" binutils/Makefile.in)
      override-configure: `(--disable-nls
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
			                      ,(elconc '--program-prefix= target-triple '-)
			                      ,(elconc '--build= $build-triple)
			                      ,(elconc '--target= target-triple)
			                      ,(elconc '--host= $triple)
			                      ,(elconc '--with-sysroot= (triple->sysroot target-triple)))
      cleanup: (elif*
	              '(rm -rf /out/usr/include)
	              '(rm -rf /out/include)
	              '(rm -rf /out/usr/lib)
	              '(rm -rf /out/lib))))))

;; this is a hack that allows us to build a cross-gcc
;; without circular dependencies: install a fake set of
;; object files that define the same symbols that musl does
(define fake-musl-for-triple
  (memoize-eq
    (lambda (target-triple)
      (lambda (host)
	      (when (eq? target-triple ($triple host))
	        (error "fake-musl for build machine will cause a bootstrap loop"))
        (let* ((hash "GpFZDYJsPUIYcsfZe-22Qk90kJ9albSjYSf_qTfzuuA=")
               (leaf (remote-archive (cdn-url hash) hash kind: 'tar.zst))
               (version '0.1))
          (package-template
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
		                (elif*
		                 `(make ,(conc "AS=" target-triple "-as")
			                  ,(conc "AR=" target-triple "-ar") all)
		                 `(make PREFIX=/usr ,(conc "DESTDIR=" outdir) install)))))))))

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
        patches: (patchfiles* "patches/gcc/pie-gcc.patch")
        tools: (list byacc reflex gawk)
        ;; we depend on cc-for-build automatically,
        ;; so we only need additional target tools
        ;; if target!=build
        cross: (list (if-native-target?
                      target-triple
                      '()
                      (list (fakelibc target-triple)
                            (binutils-for-triple target-triple))))
        libs: (list libgmp libmpfr libmpc libisl zlib)
        out-of-tree: #t
        extra-cflags: '(-DCROSS_DIRECTORY_STRUCTURE)
        native-cc: $cc-env/for-build
        env:  (list $make-overrides
                    '(gcc_cv_no_pie . no)
                    '(gcc_cv_c_no_pie . no)
                    '(gcc_cv_c_no_fpie . no))
        prepare: (elif*
                  '(find "." -name Makefile.in -exec sed "-i"
                         -e "/^AR = ar/d"        ; please don't hard-code AR
                         -e "/^ARFLAGS = cru/d"  ; please don't hard-code ARFLAGS
                         "{}" ";")
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
                  '(sed "-i"
                        -e "s/\\$(MAKE) \\$(BASE_FLAGS_TO_PASS) \\$(EXTRA_BUILD_FLAGS)/\\$(MAKE)/g"
                        Makefile.in)
                  ;; don't pull in GNU tar just to install headers;
                  ;; force the makefile to use busybox cpio instead
                  '(sed "-i"
                        -e "s/=install-headers-tar/=install-headers-cpio/g"
                        gcc/config.build))
        override-configure: `(--prefix=/usr
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
                              ,(conc   '--with-sysroot= (triple->sysroot target-triple))
                              ,(elconc '--build= $build-triple)
                              ,(conc   '--target= target-triple)
                              ,(elconc '--host= $triple))
        cleanup: (list
                  (if-native-target?
                   target-triple
                   ;; native targets still shouldn't keep /usr/bin/gcc, etc.;
                   ;; we'll install those as symlinks when native-cc is required
                   `(find /out/usr/bin -type f ! -name ,(conc target-triple "*") -delete)
                   ;; only the native version of gcc should have
                   ;; the python gdb helpers
                   '(elglob dir "/out/usr/share/gcc-*/python"
			                      rm -rf $dir))))))))

(define (busybox/config config-path extra-inputs)
  (cc-package
   "busybox" "1.32.0"
   "https://busybox.net/downloads/$name-$version.tar.bz2"
   "huLCgRsdLYg2mHLwb9_ogCACO_d0SuJXKKeqSk15XII="
   extra-src: (list (bind config-path "/src/config.head"))
   libs:      extra-inputs
   tools:     (list bzip2)
   use-native-cc: #t
   env:     (list
             '(KCONFIG_NOTIMESTAMP . 1))
   build:   (elif*
             '(mv /src/config.head .config)
             `(make V=1
                ,(elconc 'CROSS_COMPILE= $cross-compile)
                ,(elconc 'CONFIG_SYSROOT= $sysroot)
                ,(el= 'CONFIG_EXTRA_CFLAGS= $CFLAGS)
                ,$cc-env/for-kbuild
                busybox)
             '(make V=1 busybox.links)
             '(install -D -m "755" busybox /out/bin/busybox)
             '(mkdir -p /out/usr/bin /out/sbin /out/usr/sbin)
             '(redirfd -r 0 busybox.links
                       forstdin -o 0 link
                       importas "-i" -u link link
                       ln -s /bin/busybox "/out/${link}"))))

;; busybox-core is just enough busybox to build packages;
;; it doesn't include system utilities that would require
;; linux headers
(define busybox-core
  (busybox/config
   "patches/busybox/config-core" ; OE8osvZRzHk6NO3aMhnF6uyZUwlpYZtOz8LF8bR2V6k=
   '()))

(define *linux-version* "5.4.48")
(define *linux-hash* "91YnLvmAd-Jane6ZKw9huZVRz5yeEORW8bkw4mX9LCA=")

(define (linux-source version hash)
  (remote-archive
   (url-translate
    "https://cdn.kernel.org/pub/linux/kernel/v5.x/linux-$version.tar.xz"
    "linux" version)
   hash))

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
  (package-template
   src:    (linux-source *linux-version* *linux-hash*)
   dir:    (conc "linux-" *linux-version*)
   label:  (conc "linux-headers-" *linux-version*)
   tools:  (list native-toolchain)
   inputs: '()
   build:  (elif*
            `(make ,(elconc 'ARCH= (o linux-arch-name $arch))
               ,$cc-env/for-kbuild
               headers)
            ;; headers_install uses rsync, which is a
            ;; silly large dependency to pull in
            ;; at this stage...
            '(cp -r usr /out)
            '(find /out -type f ! -name "*.h" -delete))))

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
;; common fixes to scripts/dtc between linux and uboot;
;; the dtc yacc+lex files do not play nicely with BSD yacc+lex
(define (fix-dtc-script #!key
                        (fix-lex-options #f)  ;; scripts/kconfig/<foo>.l
                        (fix-yacc-cmdline #f)) ;; scripts/Makefile.host or scripts/Makefile.lib
  (unless (and fix-lex-options fix-yacc-cmdline)
    (error "fix-dtc-script is missing required keyword arguments"))
  (elif*
   `(sed "-i" -e "/^%option/s/ full / /" ,fix-lex-options)
   '(sed "-i" -e "3a override YACC:=$(YACC) -L"
         scripts/dtc/Makefile)
   '(sed "-i"
         -e "/^extern.*yyerror(/a #define YYERROR_CALL(msg) yyerror(msg)" scripts/dtc/dtc-parser.y)
   ;; byacc: use -H <file> instead of --defines=<file>
   `(sed "-i" -e "/cmd_bison/s/--defines=/-H /" ,fix-yacc-cmdline)))

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
(define (linux/config-static name config-path #!key
                             (install *default-installkernel*)
                             (dtb     #f))
  (let ((config  (bind config-path "/src/config"))
        (install (installkernel* install))
        (patches (patchfiles* "patches/dtc/lexer.patch")))
    (package-template
     src:   (append
             (list (linux-source *linux-version* *linux-hash*) install config)
             patches)
     dir:   (conc "linux-" *linux-version*)
     label: (conc "linux-" *linux-version* "-" name)
     cross: (list $cc-tools)
     tools: (list
             perl xz-tools reflex
             byacc libelf zlib linux-headers
             native-toolchain)
     inputs: '()
     env:   `((KCONFIG_NOTIMESTAMP . 1)
              (KBUILD_BUILD_TIMESTAMP . "@0")
              (KBUILD_BUILD_USER . distill)
              (KBUILD_BUILD_HOST . distill)
              (CROSS_COMPILE . ,$cross-compile))
     patches: patches
     build: (let ((make-args (list
                              $cc-env/for-kbuild
                              'YACC=yacc ;; not bison -y
                              (elconc 'ARCH= (o linux-arch-dir-name $arch))
                              "HOST_LIBELF_LIBS=-lelf -lz")))
              (elif*
               (fix-dtc-script
                fix-lex-options: 'scripts/kconfig/lexer.l
                fix-yacc-cmdline: 'scripts/Makefile.host)
               ;; libelf is built with zlib, so -lelf should imply -lz
               '(find "." -type f -name "Make*"
                      -exec sed "-i" -e
                      "s/-lelf/-lelf -lz/g" "{}" ";")
               `(make
                    V=1 KCONFIG_ALLCONFIG=/src/config
                    ,@make-args
                    allnoconfig)
               `(make V=1 ,@make-args)
	             (and dtb `(install -D -m "644" -t /out/boot ,dtb))
	             `(make V=1 ,@make-args install))))))

;; libelf is *just* libelf.a and headers;
;; it does not include the rest of elfutils
(define libelf
  (let (($cflags (lambda (conf)
                   (cons* '-D_GNU_SOURCE '-DHAVE_CONFIG_H '-I. '-I.. '-I../lib ($CFLAGS conf))))
        (config  (remote-file
                  #f "ralu1MH7h3xuq7QdjYTneOmZLEoU1RygVrTAWaKl2YY=" "/src/config.h" #o644)))
    (cc-package
     "elfutils" "0.180"
     "https://sourceware.org/$name/ftp/$version/$name-$version.tar.bz2"
     "UFOiYGMUAwV4w3uwL07JyAOZrbPB4Xcu6S34dZMVLf0="
     libs: (list zlib)
     extra-src: (list config)
     ;; the elfutils configure script is a basket case,
     ;; and so are the headers, so we're just massaging
     ;; the source and then building it manually.
     ;; (the included config.h would be generated by ./configure
     ;; if the script could actually run correctly...)

     build: (elif*
             '(cp /src/config.h config.h)
             '(find lib/ libelf/ -type f -name "*.[ch]"
                    -exec sed "-i"
                    -e "/#include <libintl.h>/d"
                    -e "/#include.*cdefs.h>/d"
                    "{}" ";")
             '(find lib/ -type f -name "*.h"
                    -exec sed "-i" -e "s/<error.h>/<err.h>/g" "{}" ";")
             `(cd libelf/
                  elglob -s csrc "*.c"
                  if (,$CC ,$cflags -c $csrc)
                  elglob -s objs "*.o"
                  ,$AR ,$ARFLAGS libelf.a $objs)
             '(mkdir -p /out/usr/include /out/usr/lib)
             '(cp libelf/gelf.h /out/usr/include/gelf.h)
	           '(cp libelf/libelf.h /out/usr/include/libelf.h)
	           '(cp libelf/libelf.a /out/usr/lib/libelf.a)))))

(define %e2fsprogs
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
     "e2fsprogs" "1.45.6"
     "https://kernel.org/pub/linux/kernel/people/tytso/$name/v$version/$name-$version.tar.xz"
     "JPnv_uv4MebWFHZWvwghtda0O-l8X3OF0m7yp16-1mI="
     patches: (patchfiles* "patches/e2fsprogs/repro.patch")
     native-cc: $buildcc-env
     libs: (list linux-headers)
     ;; can't just do $BUILD_CC foo.c;
     ;; need to actually use the specified CFLAGS!
     prepare: `(sed "-i" -e "s/-o/\\$BUILD_CFLAGS -o/" config/parse-types.sh)
     ;; atypical: e2fsprogs needs BUILD_CC, etc.
     ;; to be specified explicitly as arguments to ./configure
     extra-configure: `(--enable-symlink-install
                        --enable-libuuid
                        --enable-libblkid
			                  --disable-uuidd
			                  --disable-fsck
			                  ,$buildcc-env)
     override-install: '("MKDIR_P=install -d" DESTDIR=/out install install-libs))))

(define e2fsprogs
  (subpackage "tools-" %e2fsprogs
              "./usr/sbin/" "./usr/bin/" "./etc/*.conf"))

(define perl
  ;; the perl configure script desperately
  ;; wants to put date(1) output into the build output,
  ;; so just overwrite the date(1) executable
  (let ((samedate
         (interned "/bin/samedate" #o755
                   (lambda ()
                     (write-exexpr
                      '(echo "Fri Apr 3 20:09:47 UTC 2020")
                      shebang: "#!/bin/execlineb -s0")))))
    (cc-package
     "perl" "5.30.2"
     "https://www.cpan.org/src/5.0/$name-$version.tar.gz"
     "1LHYN7a4aIRWLowcJRJWNNTMSLl5btVOydsEyFCL2Yo="
     tools: (list samedate)
     libs:  (list libbz2 zlib)
     env:   `((BUILD_ZLIB . 0)
              (BUILD_BZIP2 . 0)
              (BZIP2_LIB . ,(elpath $sysroot "/usr/lib"))
              (BZIP2_INCLUDE . ,(elpath $sysroot "/usr/include")))
     build: (elif*
             '(ln -sf /bin/samedate /bin/date)
             `(./Configure -des
                           ,(el= '-Dcc= $CC)
                           ,(el= '-Dccflags= $CFLAGS)
                           ,(el= '-Dar= $AR)
                           ,(el= '-Darflags= $ARFLAGS)
                           ,(el= '-Dld= $LD)
                           ,(el= '-Dldflags= $LDFLAGS)
                           -Doptimize=-Os ;; TODO: pull this out of $CFLAGS
                           ,(el= '-Dnm= $NM)
                           ,(el= '-Dranlib= $RANLIB)
                           ,(el= '-Dsysroot= $sysroot)
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
     patches:   (patchfiles* "patches/dtc/lexer.patch")
     extra-src: (list envfile dotconf)
     tools:     (list byacc reflex)
     build: (let ((make-args `("YACC=yacc -d"
                               ,(elconc 'CONFIG_BOOTCOMMAND= bootcmd)
                               ,(elconc 'CROSS_COMPILE= $cross-compile)
                               ,$cc-env/for-kbuild)))
              (elif*
               `(importas -D 0 epoch SOURCE_DATE_EPOCH
                          backtick SOURCE_DATE_EPOCH (pipeline (echo -n $epoch) sed -e "s/@//"))
               '(cp /src/uboot-config .config)
               (fix-dtc-script
                fix-lex-options: 'scripts/kconfig/zconf.l
                fix-yacc-cmdline: 'scripts/Makefile.lib)
               ;; busybox xxd doesn't support '-i'; emulate it with hexdump
               '(sed "-i"
                     -e "s/xxd -i/hexdump -v -e '\\/1 \"0x%X, \"'/g"
                     -e "s/echo \", 0x00\"/echo \" 0x00\"/g" Makefile)
               ;; we're using yacc -d, so the zconf.tab.c needs to #include the generated definitions
	             '(sed "-i"
		                 -e "/^#include \"/a #include \"zconf.tab.h\"" scripts/kconfig/zconf.y)
	             `(make V=1 ,@make-args)
	             '(install -D -m 644 -t /out/boot u-boot.bin))))))

(define xz-utils
  (cmmi-package
   "xz" "5.2.5"
   "https://tukaani.org/$name/$name-$version.tar.xz"
   "-jw6foy_zDVKZ8pXQV2O2i1_6Zcs3efYVg31jflUtcQ="))

(define xz-tools (binaries xz-utils))
(define liblzma (libs xz-utils))

(define %lz4
  (cc-package
   "lz4" "1.9.2"
   ;; TODO: github release tarballs are not stable
   "https://github.com/lz4/$name/archive/v$version.tar.gz"
   "uwHhgT74Tk7ds0TQeFZTodoI1_5IZsRnVRNNHi7ywlc="
   build: (elif*
           `(make DESTDIR=/out PREFIX=/usr ,$cc-env ,$make-overrides install)
           (list $strip-cmd))))

(define lz4 (binaries %lz4))
(define liblz4 (libs %lz4))

(define %zstd
  (cc-package
   "zstd" "1.4.5"
   ;; note: this "release" tarball differs from the automated
   ;; github download release only in gzip timestamp
   ;; (decompressed content is identical)
   "https://github.com/facebook/zstd/releases/download/v$version/zstd-$version.tar.gz"
   "d9Na3jBddIFbIronfi6bQz9FAUPgPZSW5HTaxZlNrlg="
   build: (let ((makeflags '(HAVE_PTHREAD=1
                             HAVE_ZLIB=0
                             HAVE_LZMA=0
                             HAVE_LZ4=0
                             ZSTD_LEGACY_SUPPORT=0
                             ZSTD_LIB_DEPRECATED=0)))
            (elif*
             `(cd lib/
                  make PREFIX=/usr DESTDIR=/out ,$cc-env ,$make-overrides ,@makeflags install-static install-includes)
             `(cd programs/
		              make ,$cc-env ,$make-overrides ,@makeflags zstd)
	           '(install -D -m "755" programs/zstd /out/usr/bin/zstd)))))

(define zstd (binaries %zstd))
(define libzstd (libs %zstd))

(define squashfs-tools
  (let ((ver "4.4"))
    (cc-package
     "squashfs-tools" ver
     ;; TODO: github release tarballs are not stable...
     "https://github.com/plougher/$name/archive/$version.tar.gz"
     "o-ja9XdUjDj8KcrNOfKi7jQ1z37f7dtf3YUFgqRTIuo="
     ;; non-standard directory:
     dir:   (string-append "squashfs-tools-" ver "/squashfs-tools")
     env:   (list $cc-env)
     libs:  (list libzstd liblz4 liblzma zlib)
     build: (elif*
             `(make XZ_SUPPORT=1 LZO_SUPPORT=0
                    LZ4_SUPPORT=1 ZSTD_SUPPORT=1 XATTR_SUPPORT=0 ,$make-overrides)
             '(mkdir -p /out/usr/bin)
             '(cp mksquashfs unsquashfs /out/usr/bin)
             (list $strip-cmd)))))

(define libressl
  (cmmi-package
   "libressl" "3.2.0"
   "https://ftp.openbsd.org/pub/OpenBSD/LibreSSL/$name-$version.tar.gz"
   "tekDV77_P7E1zG9efeE2gENaGd6nDZd4Q9lk_lkLuCg="
   cleanup: '(ln -s openssl /out/usr/bin/libressl)))

(define libarchive+tools
  (cmmi-package
   "libarchive" "3.4.3"
   "https://github.com/libarchive/$name/releases/download/v$version/$name-$version.tar.gz"
   "g5Ye1us8NuxJl7fMXxjVGmySIr97QigjcLTjHe9tn7A="
   libs: (list libbz2 zlib liblzma liblz4 libressl libzstd)
   extra-configure: '(--without-xml2 --without-acl --without-attr --without-expat)))

(define libarchive (libs libarchive+tools))
(define bsdtar (binaries libarchive+tools))

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
  (let ((config.mk (lambda (conf)
                     (list
                      byacc reflex
                      (interned
                       "/src/config.mk"
                       #o644
                       (lines
                        (list
                         (conc "CC=" ($CC conf))
                         (conc "LDFLAGS=" (join-with " " ($LDFLAGS conf)))
                         (conc "AR=" ($AR conf))
                         "YACC=yacc"
                         "TC_CONFIG_IPSET:=y"
                         "TC_CONFIG_NO_XT:=y"
                         "HAVE_MNL:=y"
                         "CFLAGS += -DHAVE_ELF -DHAVE_SETNS -DHAVE_LIBMNL"
                         "LDLIBS += -lelf -lmnl -lz"
                         "%.o: %.c"
                         "\t$(CC) $(CFLAGS) -c -o $@ $<")))))))
    (cc-package
     "iproute2" "5.6.0"
     "https://kernel.org/pub/linux/utils/net/$name/$name-$version.tar.xz"
     "bziQSr_HXdEGLJPNH0jdUOKiuehV0HY1KI95UkV4cC0="
     patches: (patchfiles*
               "patches/iproute2/musl-fixes.patch"
               "patches/iproute2/fix-install-errors.patch")
     cross: (list config.mk)
     libs:  (list linux-headers iptables libmnl libelf zlib)
     build: (elif*
             '(cp /src/config.mk config.mk)
             '(sed "-i" -e "/^SUBDIRS/s: netem::" Makefile)
             `(make ,(el= 'CCOPTS= $CFLAGS) SHARED_LIBS=n PREFIX=/usr all)
             '(make SHARED_LIBS=n DESTDIR=/out PREFIX=/usr install)
             '(rm -rf /out/usr/share/bash-completion)
             '(rm -rf /out/var)
             '(rm -rf /out/usr/share/man)
		         (list $strip-cmd)))))

(define libs6+tools
  (ska-cmmi-package
   "s6" "2.9.2.0"
   "OSuPaQ4eMzzIAHCi4_ZM5h1e7Rv1AxXp5pE2GnBu2Ng="
   libs: (list skalibs libexecline)
   extra-configure: `(,(elconc '--with-sysdeps= $sysroot '/lib/skalibs/sysdeps)
                      --enable-static-libc)))

(define s6 (binaries libs6+tools))
(define libs6 (libs libs6+tools))

(define libs6rc+tools
  (ska-cmmi-package
   "s6-rc" "0.5.1.2"
   "y8awbxd6B7btH4qmlyx2FWwhdlysT4n19Twc-x0lotc="
   libs: (list libs6 skalibs libexecline)
   extra-configure: `(,(elconc '--with-sysdeps= $sysroot '/lib/skalibs/sysdeps)
                      --enable-static-libc)))

(define s6-rc (binaries libs6rc+tools))
(define libs6rc (libs libs6rc+tools))

;; hard(8) command; an alternative to busybox halt(8)/reboot(8)/poweroff(8)
(define hard
  (let ((hash "aVGnVsRk_al4CfeliyuIZsyj7LBG-GphmUM-BgHad7E="))
    (cc-package
     "hard" "0.1"
     (cdn-url hash)
     hash
     build: (elif*
             `(make ,(el= 'CC= $CC) ,(el= 'CFLAGS= $CFLAGS) ,(el= 'LDFLAGS= $LDFLAGS) DESTDIR=/out install)
             (list $strip-cmd)))))

(define busybox-full
  (busybox/config
   "patches/busybox/config-full" ; kHCLlhEuZrIcR3vjYENuNyI1a0eGB1B6APiyWjvkvok=
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
                     prebuilt: (cdr (assq *this-machine* *prebuilts*))
                     bootstrap: #f
                     build: #f))
	         (stage1  (config*
		                 bootstrap: stage0
		                 build: #f)))
      (config* build: #f bootstrap: stage1))))

(define dosfstools
  (cmmi-package
   "dosfstools" "4.1"
   "https://github.com/dosfstools/$name/releases/download/v$version/$name-$version.tar.gz"
   "a1HIBRNaQ-39GorXwCXTMLs-rZjAqaQNf0cbD-iqRr4="
   libs: (list linux-headers)
   extra-configure: '(--without-udev --without-iconv)))

(define mtools
  (cmmi-package
   "mtools" "4.0.24"
   "https://ftp.gnu.org/gnu/$name/$name-$version.tar.gz"
   "TDTnHIdZ2Sr-dDRK4r7Jus0HR0WaSkVypAykJJAsU0o="))

(define imgtools
  (let ((hash "q_64uUxDhbgZf5FD5aQLWUIhehbsjahGBVTjKkyzqMk="))
    (cc-package
     "imgtools" "0.3.0"
     (cdn-url hash)
     hash
     libs: (list linux-headers)
     build: (elif*
             `(make ,(el= 'CC= $CC) ,(el= 'CFLAGS= $CFLAGS) ,(el= 'LDFLAGS= $LDFLAGS) DESTDIR=/out install)
             (list $strip-cmd)))))

(define nasm
  (cmmi-package
   "nasm" "2.14.02"
   "https://www.nasm.us/pub/nasm/releasebuilds/$version/$name-$version.tar.gz"
   "TcHpCBPswC-Ee4VmIN26EzJDKsr0znrTJljcDM0TxiM="))

(define mlb2
  (let ((hash "4v7UmuN_yWWdeYzF8L2G32yjeTIV9rwTFe1nyeNvl6E="))
    (cc-package
     "mlb2" "0.2"
     (cdn-url hash)
     hash
     tools: (list nasm)
     build: (elif*
             `(make ,(el= 'CC= $CC) ,(el= 'LD= $LD) ,(el= 'CFLAGS= $CFLAGS) ,(el= 'LDFLAGS= $LDFLAGS) DESTDIR=/out install)
             (list $strip-cmd)))))
