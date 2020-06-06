(import
  scheme
  (only (chicken string) conc)
  (only (chicken port) with-output-to-string)
  (chicken module)
  (distill plan)
  (distill execline)
  (distill package)
  (distill kvector)
  (distill memo)
  (distill base))

(export
 libchicken
 egg
 matchable-egg
 srfi-13-egg
 srfi-14-egg
 srfi-69-egg)

;; TODO: add support for just compiling
;; libchicken for targets that do not need
;; csi(1), csc(1), etc.

(define (without syms lst)
  (if (null? lst)
    lst
    (let ((head (car lst)))
      (if (memq head syms)
        (cdr lst)
        (cons head (without syms (cdr lst)))))))

(define ($chicken-arch conf)
  (let ((arch ($arch conf)))
    (case arch
      ((x86_64) 'x86-64)
      (else     arch))))

(define (%chicken-src name targets install)
  (cc-package
   "chicken" "5.2.0"
   "https://code.call-cc.org/releases/$version/$name-$version.tar.gz"
   "M4edTDnvYOsgcWLxIoMpBTE0bXpG3wS3BmfiHiJ9_uA="
   build: (let* ((native  (lambda (kw)
			    (lambda (conf)
			      (kref (cc-toolchain-env ($native-toolchain conf)) kw))))
		 ($nat-cc      (native CC:))
		 ($nat-cflags  (native CFLAGS:))
		 ($nat-ldflags (native LDFLAGS:))
		 ($nat-cxx     (native CXX:))
		 ($nat-ar      (native AR:))
		 ($cflags (lambda (conf)
			    (append (without '(-fstack-protector
					       -fstack-protector-strong
					       -fstack-protector-all
					       -Os -Og -O0 -O1 -O2 -O3)
					     ($CFLAGS conf))
				    '(-fno-strict-aliasing -fwrapv -DHAVE_CHICKEN_CONFIG_H))))
		 (makeflags `(PREFIX=/usr
			      STATICBUILD=1
			      PLATFORM=linux
			      DESTDIR=/out
			      OPTIMIZE_FOR_SPEED=1
			      ;; by default, chicken is built to be a 'native'
			      ;; chicken, so it builds binaries for the host architecture
			      ;; using the 'native' C toolchain
			      ,(el= 'TARGET_C_COMPILER= $nat-cc)
			      ,(el= 'TARGET_CXX_COMPILER= $nat-cxx)
			      ,(el= 'TARGET_LIBRARIAN= $nat-ar)
			      ,(el= 'TARGET_LINKER_OPTIONS= $nat-ldflags)
			      ;; TODO: these command-line arguments
			      ;; will only work with GCC and clang;
			      ;; I don't think chicken will build with anything else anyhow...
			      ,(el= 'TARGET_C_COMPILER_OPTIONS= $nat-cflags '-fno-strict-aliasing '-fwrapv '-DHAVE_CHICKEN_CONFIG_H)
			      "LINKER_EXECUTABLE_OPTIONS=-static-pie -L."))
		 (overrides (list
			     (el= 'ARCH= $chicken-arch)
			     (el= 'C_COMPILER= $CC)
			     (el= 'CXX_COMPILER= $CXX)
			     (el= 'LIBRARIAN= $AR)
			     (el= 'LIBRARIAN_OPTIONS= $ARFLAGS)
			     (el= 'C_COMPILER_OPTIONS= $cflags)
			     (el= 'LINKER_OPTIONS= $LDFLAGS))))
	    (elif*
	     `(make ,@makeflags ,@overrides chicken-config.h)
	     `(make ,@makeflags ,@overrides ,@targets)
	     `(make ,@makeflags ,@overrides ,@install)
	     '(rm -rf /out/usr/share)))))

(define chicken
  (%chicken-src "chicken" '(all) '(install)))

(define libchicken
  (%chicken-src "libchicken" '(libchicken.a) '(install-dev)))

;; chicken-wrappers are wrappers for chicken-install and csc
;; named ${triple}-chicken-install and ${triple}-csc that
;; tweak CSC_OPTIONS so that eggs and other programs can
;; be cross-compiled correctly
(define (chicken-wrappers conf)
  (let* ((real-features (case ($arch conf)
			    ((x86_64)
			     '(64bit little-endian x86-64))
			    ((aarch64)
			     '(64bit little-endian arm64 aarch64))
			    ((armv7)
			     '(32bit litte-endian arm))
			    ((ppc64)
			     '(64bit big-endian ppc64))
			    ((ppc64le)
			     '(64bit little-endian ppc64le))))
	 (clear-features '(64bit 32bit big-endian little-endian x86-64 aarch64 x32 i386 arm ppc64))
	 (fscript        (interned
			  (conc "/usr/lib/chicken/" ($triple conf) "-features.scm")
			  #o644
			  (with-output-to-string
			    (lambda ()
			      (write `((import (chicken platform))
				       (unregister-feature! ,@clear-features)
				       (register-feature! ,@real-features)))))))
	 (splat*         (lambda (flag lst)
			   (let loop ((lst lst))
			     (if (null? lst)
				 lst
				 (cons flag (cons (car lst)
						  (loop (cdr lst))))))))
	 (csc-opts       `(-cc ,($CC conf)
			       ,@(splat* '-C ($CFLAGS conf))
			       -ld ,($CC conf) ;; chicken wants LD=CC
			       ,@(splat* '-L ($LDFLAGS conf))
			       ,@(splat* '-no-feature (without real-features clear-features))
			       ,@(splat* '-feature real-features)))
	 (csc-script     (interned
			  (conc "/usr/bin/" ($triple conf) "-csc")
			  #o755
			  (lambda ()
			    (write-exexpr
			     `(export CSC_OPTIONS ,(spaced csc-opts)
				      csc "$@")
			     shebang: "#!/bin/execlineb -s0"))))
	 (ckn-install    (interned
			  (conc "/usr/bin/" ($triple conf) "-chicken-install")
			  #o755
			  (lambda ()
			    (write-exexpr
			     `(export CSC_OPTIONS ,(spaced csc-opts)
				      chicken-install "$@")
			     shebang: "#!/bin/execlineb -s0")))))
    (list csc-script ckn-install)))

;; egg defines a package for an egg
(define (egg name version hash . deps)
  ;; using cc-package here means
  ;; we pick up the correct libc and C compiler
  ;; automagically; we should honor those parts of <config>
  (cc-package
   name version
   "https://code.call-cc.org/egg-tarballs/5/$name/$name-$version.tar.gz"
   hash
   tools: (lambda (conf) (append (cons chicken (chicken-wrappers conf)) deps))
   libs:  (cons libchicken deps)
   build: `(backtick
	    -n "-i" repo (chicken-install -repository)
	    importas "-i" -u repo repo
	    export TMP /tmp
	    export CHICKEN_INSTALL_REPOSITORY "/out/${repo}"
	    ;; -host is just tricking chicken-install ;
	    ;; into compiling statically ;
	    ,(elconc $triple '-chicken-install) -host -no-install-dependencies)))

(define matchable-egg
  (egg "matchable" "1.1" "rygA7BrZVhDdv2x9ksqaK0fgKIc-nYT9unDIHOYlQI4="))

(define srfi-14-egg
  (egg "srfi-14" "1.4" "NwafERqp27VVghFvWoyqQooQdJ7-evsJVx-KA6Q3-7I="))

(define srfi-13-egg
  (egg "srfi-13" "1.4" "ceY9c3sFKZVzpJZT4Z6uCqNgZAH81hbl0Di8OX3zAik="
       srfi-14-egg))

(define srfi-69-egg
  (egg "srfi-69" "1.2" "aJQMPPCpKVg_53zI_5YzvK6w-ZWwGaeCORJtAjM_Fyc="))
