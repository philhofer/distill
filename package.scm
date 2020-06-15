(: *this-machine* symbol)
(define *this-machine*
  (cond-expand
   (x86-64 'x86_64)
   (arm64  'aarch64)
   ;; TODO: additional verification that this is armv7-a
   (arm    'armv7)
   ((and ppc64 little-endian) 'ppc64le)
   ((and ppc64 big-endian) 'ppc64)))

;; package-template is a helper
;; for writing package definitions
(define (package-template
	 #!key
	 label
	 build
	 (src '())
	 (cross '())
	 (tools '())
	 (inputs '())
	 (patches '())
	 (dir "/")
	 (env '())
	 (raw-output #f)
	 (cc-tools   #t))
  (lambda (host)
    (let* ((name      (conc label "-" ($arch host)))
	   (bld       ($build host))
	   (strap     (%bootstrap bld))
	   (ex        (%current-expander))
	   (boot      ($cc-tools bld))
	   ;; tools are built with the 'build' config,
	   ;; unless they are part of the build config toolchain,
	   ;; in which case they are built with the bootstrap config
	   (exp/build (lambda (item)
			(cond
			 ((artifact? item) item)
			 ((plan? item) item)
			 ((memq item boot)
			  (if strap
			      (ex item strap)
			      (error "cannot expand item (no bootstrap in <config>)" item)))
			 (else (ex item bld)))))
	   (uncross   (lambda (item)
			(cond
			 ((artifact? item) item)
			 ((plan? item) item)
			 ((procedure? item)
			  (item host))
			 (else (error "cannot expand item in 'cross:'" item)))))
	   (exp/host  (lambda (item)
			(ex item host)))
	   (->tool  (lambda (link)
		      (make-input
		       basedir: "/"
		       link: link)))
	   (->input (let ((sysroot ($sysroot host)))
		      (lambda (link)
			(if (input? link)
			    link
			    (make-input
			     basedir: sysroot
			     link: link))))))
      (push-exception-wrapper
       (lambda (exn)
	 (let* ((get (condition-property-accessor 'exn 'call-chain))
		(ch  (and get (get exn))))
	   (when ch
	     (for-each
	      (lambda (e) (info "  <trace>" (vector-ref e 0)))
	      ch)))
	 (print-error-message exn)
	 (fatal "error while expanding" label)
	 exn)
       (lambda ()
	 (make-plan
	  raw-output: raw-output
	  name:       label
	  inputs: (cons
		   (make-input
		    basedir: "/"
		    link: (buildfile host dir env patches build))
		   (append
		    (map ->tool (expandl exp/build
					 (flatten src tools (map uncross cross))))
		    (map ->input (expandl exp/host inputs))))))))))

(define (buildfile conf dir env patches build)
  (interned
   "/build" #o744
   (lambda ()
     (write-exexpr
      (cons*
       'cd dir
       (elpatch
	patches
	(exports->script
	 conf
	 env
	 (cond
	  ((list? build)
	   (elexpand conf build))
	  ((procedure? build)
	   (build conf))
	  (else (error "expected build; got" build))))))))))

;; new-memoizer creates a new memoizer for package expansion
(define (new-memoizer)
  (let ((expand (memoize-lambda (obj conf) (obj conf))))
    (lambda (obj conf)
      (let loop ((obj obj))
	(cond
	 ((procedure? obj)
	  (loop (expand obj conf)))
	 ((list? obj) obj)
	 ((plan? obj) obj)
	 ((artifact? obj) obj)
	 ((input? obj) obj)
	 (else (begin
		 (import (chicken pretty-print))
		 (pp obj)
		 (error "unexpected expansion value" obj))))))))

;; %current-expander is the current memoization
;; context, which is used to recursively expand
;; package lambdas into plans an artifacts
(define %current-expander (make-parameter #f))

;; (configure x conf) expands x with 'conf'
;; using the current expansion environment
(: configure (* vector -> *))
(define (configure x conf) ((%current-expander) x conf))

;; expander returns a monoid that expands
;; lambdas into plan or artifact objects
(: expander (vector -> (* -> *)))
(define (expander host)
  (let ((memo (new-memoizer)))
    (lambda (obj)
      (parameterize ((%current-expander memo))
	(memo obj host)))))

(: subpackage (string * #!rest string -> (vector -> vector)))
(define (subpackage prefix sub . globs)
  (lambda (conf)
    (let ((child (configure sub conf)))
      (make-plan
       name: (string-append
	      prefix
	      (if (plan? child)
		  (plan-name child)
		  "unknown"))
       inputs: (list
		(make-input
		 basedir: "/out"
		 link: child
		 wrap: (lambda (art)
			 (sub-archive art globs))))
       null-build: #t))))

;; clibs wraps a package and yields
;; only the parts of its outputs that
;; are in conventional locations for C library files
(define (libs pkg)
  (subpackage "lib-" pkg
	      "./usr/lib/" "./lib/"
	      "./usr/include/" "./include/"
	      "./usr/share/"))

;; binaries produces a subpackage
;; by matching common binary directories
(define (binaries pkg)
  (subpackage "bin-" pkg
	      "./usr/bin/" "./bin/"
	      "./usr/sbin/" "./sbin/"
	      "./usr/libexec/" "./libexec/"
	      "./usr/share/" "./share/"))

(define (url-translate urlfmt name version)
  (string-translate* urlfmt `(("$name" . ,name)
			      ("$version" . ,version))))

;; template for C/C++ packages
;;
;; the cc-package template picks suitable defaults
;; for the package source, inputs (libc), tools (toolchain)
;; and build directory; the additional keyword arguments
;; can be used to augment that package template with
;; more libraries and tools
;;
;; the 'build:' argument is mandatory; cc-package
;; does not provide a default build script
(define (cc-package
	 name version urlfmt hash
	 #!key
	 (dir #f)        ;; directory override
	 (build #f)
	 (prebuilt #f)   ;; prebuilt function
	 (no-libc #f)	 ;; do not bring in a libc implicitly
	 (use-native-cc #f)
	 (raw-output #f)
	 (patches '())   ;; patches to apply
	 (env '())	 ;; extra environment
	 (libs '())	 ;; extra libraries beyond libc
	 (tools '())	 ;; extra tools beyond a C toolchain
	 (extra-src '()))
  (let* ((url (url-translate urlfmt name version))
	 (src (remote-archive url hash))
	 ($native-libc  (o cc-toolchain-libc $native-toolchain))
	 ($native-tools (o cc-toolchain-tools $native-toolchain)))
    (package-template
     raw-output: raw-output
     cc-tools: #t
     patches: patches
     label:   (conc name "-" version)
     src:     (cons src (append patches extra-src))
     env:     env
     dir:     (or dir (conc name "-" version))
     cross:   (list $cc-tools)
     tools:   (if use-native-cc
		  (cons* $cc-tools $libc $native-libc $native-tools
			 (if (list? tools) tools (list tools)))
		  tools)
     inputs:  (if no-libc libs (cons $libc libs))
     build:   (if (procedure? build) build (eltemplate build)))))

;; template for 'configure; make; make install;' or 'c;m;mi'
;; that has sane defaults for environment, tools, libraries
;; (see cc-package) and a reasonable default for the build script.
;;
(define (cmmi-package
	 name version urlfmt hash
	 #!key
	 (dir #f)    ;; directory override
	 (prebuilt #f) ;; prebuilt function
	 (patches '()) ;; patches to apply
	 (env '())	 ;; extra environment

	 ;; override-configure, if not #f, takes
	 ;; precedence over the default configure options;
	 ;; otherwise (append default extra-configure) is used
	 (extra-configure '())
	 (override-configure #f)

	 (override-make #f)
	 (override-install #f)
	 (out-of-tree #f)

	 (libs '())	 ;; extra libraries beyond libc
	 (tools '())	 ;; extra tools beyond a C toolchain
	 (extra-src '()) ;; extra source
	 (prepare #f)   ;; a place to put sed chicanery, etc.
	 (cleanup #f)   ;; a place to put extra install tweaking
	 (native-cc #f) ;; should be one of the cc-env/for-xxx functions if not #f
	 (parallel  #t)
	 (extra-cflags '()))
  (let* ((default-configure `(--disable-shared
			      --enable-static
			      --disable-nls
			      --prefix=/usr
			      --sysconfdir=/etc
			      --build ,$build-triple
			      --host ,$triple))
	 (make-args (or override-make (list $make-overrides))))
    (cc-package
     name version urlfmt hash
     dir: dir
     patches: patches
     prebuilt: prebuilt
     libs: libs
     tools: tools
     extra-src: extra-src
     use-native-cc: (if native-cc #t #f)
     env:     (cons
	       (if (null? extra-cflags)
		   $cc-env
		   (lambda (conf)
		     (kwith ($cc-env conf)
			    CFLAGS: (+= extra-cflags)
			    CXXFLAGS: (+= extra-cflags))))
	       (if native-cc
		   (cons native-cc env)
		   env))
     build:   (append
	       (if parallel '() '(unexport MAKEFLAGS))
	       (if prepare (list 'if prepare) '())
	       (let ((args (or override-configure
			       (append default-configure
				       extra-configure))))
		 (if out-of-tree
		     `(if (mkdir -p distill-builddir)
			  cd distill-builddir
			  if (../configure ,@args))
		     `(if (./configure ,@args))))
	       `(if
		 (make ,@make-args)
		 if (make ,@(or override-install '(DESTDIR=/out install)))
		 foreground (rm -rf /out/usr/share/man /out/usr/share/info)
		 foreground (find /out -name "*.la" -delete))
	       (if cleanup (list 'if cleanup) '())
	       (list $strip-cmd)))))

(: <cc-env> vector)
(define <cc-env>
  (make-kvector-type
   AR:
   AS:
   CC:
   LD:
   NM:
   CXX:
   CBUILD:
   CHOST:
   CFLAGS:
   ARFLAGS:
   CXXFLAGS:
   LDFLAGS:
   RANLIB:
   STRIP:
   READELF:
   OBJCOPY:))

(define cc-env? (kvector-predicate <cc-env>))
(define make-cc-env (kvector-constructor <cc-env>))

(define-kvector-type
  <cc-toolchain>
  make-cc-toolchain
  cc-toolchain?
  (cc-toolchain-tools tools: '() list?)
  (cc-toolchain-libc  libc:  '() list?)
  (cc-toolchain-env   env:   #f  cc-env?))

(define-kvector-type
  <config>
  make-config
  config?
  ($arch         arch:         #f symbol?)
  ($triple       triple:       #f symbol?)
  ;; C toolchain that builds for host=triple
  ($cc-toolchain cc-toolchain: #f cc-toolchain?)
  ;; C toolchain (wrappers) that builds for build=host=triple
  ($native-toolchain native-cc-toolchain: #f cc-toolchain?)
  ;; config for tool prerequisites:
  (%build         build:        #f (perhaps config?))
  ;; config for toolchain prerequisites:
  (%bootstrap     bootstrap:    #f (perhaps config?)))

(define ($build conf)
  (or (%build conf) conf))

(define ($native? conf)
  (not (%build conf)))

(define ($leaf conf)
  (cond
   ((%build conf) => $leaf)
   ((%bootstrap conf) => $leaf)
   (else conf)))

(define $build-toolchain (o $native-toolchain $build))
(define $build-triple (o $triple $build))

(define $cc-env (o cc-toolchain-env $cc-toolchain))

(define ($cross-compile conf) (conc ($triple conf) "-"))

(define $cc-tools (o cc-toolchain-tools $cc-toolchain))
(define $libc     (o cc-toolchain-libc $cc-toolchain))

(define (triple->sysroot trip)
  (string-append "/sysroot/" (symbol->string trip)))

(define ($sysroot conf)
  (triple->sysroot ($triple conf)))

(define (triple->arch trip)
  (string->symbol (car (string-split (symbol->string trip) "-"))))

(define (cenv v) (o (kvector-getter <cc-env> v) $cc-env))

(define $CC (cenv CC:))
(define $CXX (cenv CXX:))
(define $CFLAGS (cenv CFLAGS:))
(define $CXXFLAGS (cenv CXXFLAGS:))
(define $LD (cenv LD:))
(define $LDFLAGS (cenv LDFLAGS:))
(define $AR (cenv AR:))
(define $ARFLAGS (cenv ARFLAGS:))
(define $RANLIB (cenv RANLIB:))
(define $READELF (cenv READELF:))
(define $OBJCOPY (cenv OBJCOPY:))
(define $NM (cenv NM:))

;; $make-overrides is the subset of <config>
;; that is supplied as k=v arguments to invocations
;; of $MAKE (in order to override assignments in the Makefile)
(define $make-overrides
  (memoize-one-eq
   (o
    (subvector-constructor
     <cc-env>
     RANLIB: STRIP: NM: READELF: OBJCOPY: AR: ARFLAGS:)
    $cc-env)))

(define $build-CC)
(define $build-CFLAGS)
(define $build-LD)
(let ((buildenv (lambda (kw)
		  (o (kvector-getter <cc-env> kw)
		     cc-toolchain-env
		     $build-toolchain))))
  (set! $build-CC (buildenv CC:))
  (set! $build-LD (buildenv LD:))
  (set! $build-CFLAGS (buildenv CFLAGS:)))

;; cc-env/build produces a C environment
;; by transforming the usual CC, LD, etc.
;; variables using (frob-kw key) for each keyword
(: cc-env/build (vector (keyword -> keyword) -> vector))
(define (cc-env/build conf frob-kw)
  (let ((folder (lambda (kw arg lst)
		  ;; ignore cc-env values that are #f
		  (if arg
		      (cons (frob-kw kw) (cons arg lst))
		      lst))))
    (list->kvector
     (kvector-foldl
      (cc-toolchain-env ($native-toolchain ($build conf)))
      folder
      '()))))

;; cc-env/for-build is a C environment
;; with CC_FOR_BUILD, CFLAGS_FOR_BUILD, etc.
;; (this is typical for autotools-based configure scripts)
(define ($cc-env/for-build conf)
  (cc-env/build
   conf
   (lambda (kw)
     (string->keyword
      (string-append
       (keyword->string kw)
       "_FOR_BUILD")))))

;; cc-env/for-kbuild is a C environment
;; with HOSTCC, HOSTCFLAGS, etc.
;; (this is typical for Kbuild-based build systems)
(define ($cc-env/for-kbuild conf)
  (cc-env/build
   conf
   (lambda (kw)
     (string->keyword
      (string-append
       "HOST"
       (keyword->string kw))))))

(: spaced (* -> string))
(define (spaced lst)
  (let ((out (open-output-string)))
    (cond
     ((list? lst)
      (let loop ((lst lst))
	(or (null? lst)
	    (let ((head (car lst))
		  (rest (cdr lst)))
	      (if (list? head)
		  (loop head)
		  (display head out))
	      (unless (null? rest)
		      (display " " out))
	      (loop rest)))))
     (else (display lst out)))
    (let ((s (get-output-string out)))
      (close-output-port out)
      s)))

;; k=v takes a key and a value
;; and returns a string like "key=value"
;; where 'value' becomes space-separated
;; if it is a list
(: k=v (keyword * --> string))
(define (k=v k v)
  (unless (keyword? k)
	  (error "k=v expected a keyword but got" k))
  (string-append
   (##sys#symbol->string k)
   "="
   (spaced v)))

;; k=v* takes an arbitrary set of keyword: value forms
;; and converts them into a list of strings of the form keyword=value
(: k=v* (keyword * #!rest * --> (list-of string)))
(define (k=v* k v . rest)
  (let loop ((k k)
	     (v v)
	     (rest rest))
    (cons (k=v k v)
	  (if (null? rest)
	      '()
	      (let ((k  (car rest))
		    (vp (cdr rest)))
		(loop k (car vp) (cdr vp)))))))

;; splat takes a kvector and a list of keywords
;; and produces a list of key=value strings
;; for just those keywords in the kvector
(: splat (vector #!rest keyword --> (list-of string)))
(define (splat conf . keywords)
  (map
   (lambda (k)
     (k=v k (kref conf k)))
   keywords))

;; core list-expanding procedure:
;;
;; use 'proc' to expand items of 'lst',
;; where any sub-lists that are expanded
;; are recursively expanded and spliced
;; into the parent list so that the result
;; is always a flat list with no elements
;; that are 'eq?' to one another
(define (expandl proc lst)
  (let loop ((in  lst)
	     (out '()))
    (if (null? in)
	out
	(let ((head (car in))
	      (rest (cdr in)))
	  (if (memq head rest)
	      (loop rest out)
	      (let ((h (proc head)))
		(loop (cdr in)
		      (cond
		       ((list? h) (loop h out))
		       ;; deduplicate 'eq?'-equivalent items
		       ((memq h out) out)
		       (else (cons h out))))))))))

;; config->builder takes a configuration (as an alist or conf-lambda)
;; and returns a function that builds the package
;; and yields its output artifact
(define (config->builder env)
  (let* ((host   (conform config? env))
	 (expand (expander host))
	 (->out  (lambda (final)
		   (if (artifact? final)
		       final
		       (plan-outputs final)))))
    (lambda args
      (let ((plans (map expand args)))
	(or (null? plans) (build-graph! plans))
	(map ->out plans)))))

(define $strip-cmd
  (eltemplate
   `(forbacktickx
     file (find /out -type f -perm -o+x)
     importas "-i" -u file file
     backtick prefix (head -c4 $file)
     importas "-i" -u prefix prefix
     ;; the execline printing code knows
     ;; how to escape a raw byte sequence
     ;; (elf binaries begin with (0x7f)ELF)
     if (test $prefix "=" #u8(127 69 76 70))
     if (echo "strip" $file)
     ,(elconc $triple "-strip") $file)))

;; bind locates a file relative to the current working directory,
;; the current install directory, or the current install's lib directory,
;; and produces an artifact that appears at 'therepath' with the same
;; permissions as the file has locally
;;
;; TODO: handle directories
(define (bind herepath therepath)
  (let ((f (or
	    (file-exists? herepath)
	    (let ((ep (executable-pathname)))
	      (or (file-exists? (filepath-join ep herepath))
		  (file-exists? (filepath-join ep "../lib/distill/" herepath))
		  (error "couldn't find file" herepath))))))
    (overlay f therepath)))

;; patchfiles* looks relative to the current directory,
;; the current executable's directory, and finally
;; the installed lib directory for files with the given
;; names to be used as patches
(define (patchfiles* . names)
  (if (null? names)
      '()
      (let loop ((n 0)
		 (head (car names))
		 (rest (cdr names)))
	(cons
	 (bind head (conc "/src/patch-" n ".patch"))
	 (if (null? rest) '() (loop (+ n 1) (car rest) (cdr rest)))))))

;; elpatch takes a list of patches and prepends
;; the code for applying those patches to the execline
;; template 'body', i.e.
;;   (elpatch (patchfiles* ...) body ...)
(define (elpatch lst body)
  (if (null? lst)
      body
      (cons*
       'if `(patch -p1 "-i" ,(vector-ref (vector-ref (car lst) 0) 1))
       (elpatch (cdr lst) body))))

(define (exports->script conf lst body)
  ;; expand a variable as the left-hand-side
  ;; of a key=value environment variable expression
  (define (lhs x)
    (cond
     ((symbol? x) x)
     ((string? x) x)
     ((procedure? x) (lhs (x conf)))
     (else (error "env: cannot use value as literal" x))))
  (define (rhs x)
    (cond
     ((symbol? x)  x)
     ((string? x)  x)
     ((number? x)  x)
     ((list? x)      (spaced (map rhs x)))
     ((procedure? x) (rhs (x conf)))
     (else (error "env: cannot use value as env var" x))))
  (if (null? lst)
      body
      (let loop ((in  lst)
		 (exp '()))
	(if (null? in)
	    (cons* 'exportall exp body)
	    (let kons ((h    (car in))
		       (rest (cdr in)))
	      (cond
	       ((not h)
		(loop rest exp))
	       ((pair? h)
		(loop rest
		      (cons (lhs (car h)) (cons (rhs (cdr h)) exp))))
	       ((kvector? h)
		(loop rest
		      (kvector-foldl
		       h
		       (lambda (k v lst)
			 (if v (cons (##sys#symbol->string k) (cons (rhs v) lst)) lst))
		       exp)))
	       ((procedure? h)
		(kons (h conf) rest))
	       (else (error "bad env element" (car in)))))))))
