(: *this-machine* symbol)
(define *this-machine*
  (cond-expand
   (x86-64 'x86_64)
   (arm64  'aarch64)
   ;; TODO: additional verification that this is armv7-a
   (arm    'armv7)
   ((and ppc64 little-endian) 'ppc64le)
   ((and ppc64 big-endian) 'ppc64)))

(: <package> vector)
(define <package>
  (make-kvector-type
   label:
   src:
   tools:
   inputs:
   prebuilt:
   patches:
   build:
   dir:
   raw-output:))

(define <meta-package> (list 'meta-package))

;; make-metapackage is an alternative for
;; make-package that simply returns a list
;; of other packages and/or artifacts
(define (make-meta-package proc)
  (vector <meta-package> proc))

(: meta-package? (* -> boolean))
(define (meta-package? arg)
  (and (vector? arg)
       (= (vector-length arg) 2)
       (eq? (vector-ref arg 0) <meta-package>)))

(define-memoized (meta-expand mp conf)
  ((vector-ref mp 1) conf))

(: package? (* -> boolean))
(define package? (kvector-predicate <package>))

(define raw-make-package (kvector-constructor <package>))

(: make-package (#!rest * -> vector))
(define make-package
  (kvector-constructor
   <package>
   label:      #f  string?
   src:        '() (or/c artifact? (list-of artifact?))
   tools:      '() (list-of (or/c meta-package? procedure? artifact?))
   inputs:     '() (list-of (or/c meta-package? procedure? artifact?))
   prebuilt:   #f  (or/c false/c artifact?)
   patches:    '() list?
   build:      #f  list?
   dir:        "/" string?
   raw-output: #f  (or/c false/c string?)))

(define (source-template name version urlfmt hash #!optional (patchlst '()))
  (let* ((url (string-translate* urlfmt `(("$name" . ,name)
                                          ("$version" . ,version))))
         (src (remote-archive url hash)))
    (lambda (conf)
      (raw-make-package
       patches: patchlst
       label:   (conc name "-" version "-" ($arch conf))
       src:     (cons src patchlst)
       dir:     (conc name "-" version)))))

;; temporary hack
(define (source->package conf src . rest)
  (apply kupdate (src conf) rest))

(define (package-mutator . args)
  (let* ((vec (list->vector args))
         (len (vector-length vec)))
    (let loop ((i 0))
      (or (= i len)
          (begin
            (vector-set! vec i (kidx <package> (vector-ref vec i)))
            (loop (+ i 2)))))
    (lambda (pkg conf)
      (let loop ((i 0))
        (or (= i len)
            (begin
              (vector-set! pkg i ((vector-ref vec (+ i 1)) conf (vector-ref pkg i)))
              (loop (+ i 2))))))))

(define (integrate template . extensions)
  (lambda (conf)
    (let ((root (template conf)))
      (let loop ((lst extensions))
        (if (null? lst)
	    root
	    (begin
	      ((car lst) root conf)
	      (loop (cdr extensions))))))))

(: update-package (vector #!rest * -> vector))
(define (update-package pkg . args)
  (apply make-package
         (append
	  (kvector->list pkg)
	  args)))

(: package-label (vector --> string))
(define package-label (kvector-getter <package> label:))
(: package-tools (vector --> (list-of procedure)))
(define package-tools (kvector-getter <package> tools:))
(: package-src (vector --> *))
(define package-src (kvector-getter <package> src:))
(: package-inputs (vector --> (list-of procedure)))
(define package-inputs (kvector-getter <package> inputs:))
(: package-prebuilt (vector --> (or vector false)))
(define package-prebuilt (kvector-getter <package> prebuilt:))
(: package-build (vector --> list))
(define package-build (kvector-getter <package> build:))
(: package-raw-output (vector --> (or string false)))
(define package-raw-output (kvector-getter <package> raw-output:))
(: package-dir (vector --> (or string false)))
(define package-dir (kvector-getter <package> dir:))
(: package-patches (vector --> list))
(define package-patches (kvector-getter <package> patches:))

(: <config> vector)
(define <config>
  (make-kvector-type
   arch:
   triple:
   cc-toolchain:
   native-cc-toolchain:))

(: <cc-toolchain> vector)
(define <cc-toolchain>
  (make-kvector-type
   tools:
   libc:
   env:))

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

(define make-cc-toolchain
  (kvector-constructor
   <cc-toolchain>
   tools: '() (list-of (disjoin procedure? vector?))
   libc:  '() (list-of (disjoin procedure? vector?))
   env:   #f  cc-env?))

(: config? (* -> boolean))
(define config? (kvector-predicate <config>))

(: $arch (vector --> symbol))
(define $arch    (kvector-getter <config> arch:))
(: $triple (vector --> symbol))
(define $triple  (kvector-getter <config> triple:))
(: $cc-toolchain (vector --> vector))
(define $cc-toolchain (kvector-getter <config> cc-toolchain:))
(define $native-toolchain (kvector-getter <config> native-cc-toolchain:))
(: cc-toolchain-env (vector --> vector))
(define cc-toolchain-env (kvector-getter <cc-toolchain> env:))
(define cc-toolchain-tools (kvector-getter <cc-toolchain> tools:))
(define cc-toolchain-libc (kvector-getter <cc-toolchain> libc:))

(: $cc-env (vector --> vector))
(define $cc-env (o cc-toolchain-env $cc-toolchain))

;; sort of a hack to keep a circular dependency
;; between base.scm and package.scm from creeping in:
;; have package.scm declare the 'build-config'
;; parameter, but have 'base.scm' set it as soon
;; as it declares default-config
(: build-triple (-> symbol))
(define (build-triple) ($triple (build-config)))

(define (+cross conf normal extra)
  (if (eq? (build-triple) ($triple conf))
      normal
      (append extra normal)))

(: make-config (#!rest * -> vector))
(define make-config (kvector-constructor <config>))

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

;; cc-env/build produces a C environment
;; by transforming the usual CC, LD, etc.
;; variables using (frob-kw key) for each keyword
(: cc-env/build ((keyword -> keyword) -> vector))
(define (cc-env/build frob-kw)
  (let ((folder (lambda (kw arg lst)
		  ;; ignore cc-env values that are #f
		  (if arg
		      (cons (frob-kw kw) (cons arg lst))
		      lst))))
    (list->kvector
     (kvector-foldl
      (cc-toolchain-env ($native-toolchain (build-config)))
      folder
      '()))))

;; cc-env/for-build is a C environment
;; with CC_FOR_BUILD, CFLAGS_FOR_BUILD, etc.
;; (this is typical for autotools-based configure scripts)
(define (cc-env/for-build)
  (cc-env/build (lambda (kw)
		  (string->keyword
		   (string-append
		    (keyword->string kw)
		    "_FOR_BUILD")))))

;; cc-env/for-kbuild is a C environment
;; with HOSTCC, HOSTCFLAGS, etc.
;; (this is typical for Kbuild-based build systems)
(define (cc-env/for-kbuild)
  (cc-env/build (lambda (kw)
		  (string->keyword
		   (string-append
		    "HOST"
		    (keyword->string kw))))))

(: spaced (* -> string))
(define (spaced lst)
  (let ((out (open-output-string)))
    (if (list? lst)
	(let loop ((lst lst))
	  (or (null? lst)
	      (let ((head (car lst))
		    (rest (cdr lst)))
		(if (list? head)
		    (loop head)
		    (display head out))
		(unless (null? rest)
			(display " " out))
		(loop rest))))
	(display lst out))
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

;; kvargs takes a kvector
;; and produces a list of strings
;; where each string is formatted as "key=value"
;; for each key-value mapping in the kvector
(: kvargs (vector --> (list-of string)))
(define (kvargs kvec)
  (kvector-map kvec k=v))

;; splat takes a kvector and a list of keywords
;; and produces a list of key=value strings
;; for just those keywords in the kvector
(: splat (vector #!rest keyword --> (list-of string)))
(define (splat conf . keywords)
  (map
   (lambda (k)
     (k=v k (kref conf k)))
   keywords))

;; kvexport produces a list of non-terminal
;; execline expressions of the form
;;   (export <key> <value>)
;; for each element in the given kvector,
;; taking care to format values as a single
;; string, even if they are lists
(: kvexport (vector --> list))
(define (kvexport kvec)
  (kvector-map
   kvec
   (lambda (k v)
     `(export ,(##sys#symbol->string k) ,(spaced v)))))

;; build-config is the configuration
;; for artifacts produced for *this* machine (tools, etc)
;; (this is set to a reasonable value when base.scm is loaded)
(define build-config
  (make-parameter #f))

;; recursively apply 'proc' to items of lst
;; while the results are lists, and deduplicate
;; precisely-equivalent items along the way
;;
;; this is used to expand package#tools and package#inputs
;; so that meta-packages are recursively unpacked into a single
;; list of packages and artifacts
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
  (let* ((host  (conform config? env))
	 (build (build-config))
	 (->plan/art (lambda (in)
		       (cond
			((artifact? in) in)
			((meta-package? in) (meta-expand in host))
			((procedure? in) (package->plan in build host))
			(else "bad item provided to plan builder" in))))
	 (->out (lambda (pln)
		  (cond
		   ((artifact? pln) pln)
		   ((procedure? pln) (let ((plan (package->plan pln build host)))
				       (if (artifact? plan) plan (plan-outputs plan))))
		   ;; TODO: meta-packages
		   (else #f)))))
    (lambda args
      (let ((plans (expandl ->plan/art args)))
	(unless (null? plans)
		(build-graph! plans))
	(map ->out args)))))


;; package->plan is the low-level package expansion code;
;; it recursively simplifies packges into plans, taking care
;; to only expand packges once for each (build, host) configuration tuple
(define package->plan
  (memoize-lambda
   (pkg-proc build host)
   (let ((pkg (pkg-proc host)))
     (or (package-prebuilt pkg)
	 (let* ((expander (lambda (conf)
			    (lambda (in)
			      (cond
			       ((artifact? in) in)
			       ((meta-package? in) (meta-expand in conf))
			       (else (package->plan in build conf))))))
		(->tool  (expander build))
		(->input (expander host))
		(tools   (package-tools pkg))
		(inputs  (package-inputs pkg)))
	   (make-plan
	    raw-output: (package-raw-output pkg)
	    name:   (package-label pkg)
	    inputs: (list
		     (cons "/" (cons*
				(package-buildfile pkg)
				(expandl ->tool (flatten (package-src pkg) tools))))
		     ;; build headers+libraries live here
		     (cons ($sysroot host) (expandl ->input inputs)))))))))

(: package-buildfile (vector -> vector))
(define (package-buildfile r)
  (interned
   "/build" #o744
   (lambda ()
     (write-exexpr
      (let ((dir (package-dir r))
	    (patches (or (package-patches r) '())))
	(unless dir "error: no dir specified" r)
	(cons
	 `(cd ,dir)
	 (append
	  (script-apply-patches patches)
	  (package-build r))))))))

;; generator for an execline sequence that strips binaries
(define (strip-binaries-script triple)
  `((forbacktickx file ((find /out -type f -perm -o+x)))
    (importas "-i" -u file file)
    (backtick prefix ((head -c4 $file)))
    (importas "-i" -u prefix prefix)
    ;; the execline printing code knows
    ;; how to escape a raw byte sequence
    ;; (elf binaries begin with (0x7f)ELF)
    (if ((test $prefix "=" #u8(127 69 76 70))))
    (if ((echo "strip" $file)))
    (,(conc triple "-strip") $file)))

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

(define <gnu-build>
  (make-kvector-type
   triple:
   pre-configure:
   exports:
   post-install:
   make-flags:
   install-flags:
   out-of-tree:
   configure-args:))

;; $gnu-build fetches the default <gnu-build> object
;; for a configuration
(define $gnu-build
  (let ((%make (kvector-constructor <gnu-build>)))
    ;; cache one copy of this since it's likely to be
    ;; the same result in most invocations
    (memoize-one-eq
     (lambda (conf)
       (%make
	triple:         ($triple conf)
	pre-configure:  '()
	exports:        (list ($cc-env conf))
	post-install:   '()
	make-flags:     (kvargs ($make-overrides conf))
	install-flags:  '(DESTDIR=/out install)
	out-of-tree:    #f
	configure-args: `(--disable-shared
			  --disable-nls --enable-static
			  --enable-pie --with-pic
			  --prefix=/usr --sysconfdir=/etc
			  --build ,(build-triple)
			  --host ,($triple conf)))))))

(define (exports->script lst)
  (if (null? lst)
      '()
      (let ((head (car lst))
	    (rest (cdr lst)))
	(cond
	 ((pair? head)
	  (cons `(export ,(car head) ,(cdr head))
		(exports->script rest)))
	 ((kvector? head)
	  (append (kvexport head)
		  (exports->script rest)))
	 (else
	  (error "unexpected export value:" lst))))))


(define (+gnu-ccflags gr lst)
  (kwith
   gr
   exports: (lambda (old)
	      (map
	       (lambda (ex)
		 (if (cc-env? ex)
		     (kwith ex CFLAGS: (+= lst) CXXFLAGS: (+= lst))
		     ex))
	       old))))

;; gnu-recipe takes the result of ($gnu-build conf)
;; and turns it into a recipe
(define gnu-recipe
  (let (($pre-configure  (kvector-getter <gnu-build> pre-configure:))
	($exports        (kvector-getter <gnu-build> exports:))
	($post-install   (kvector-getter <gnu-build> post-install:))
	($make-flags     (kvector-getter <gnu-build> make-flags:))
	($install-flags  (kvector-getter <gnu-build> install-flags:))
	($out-of-tree    (kvector-getter <gnu-build> out-of-tree:))
	($configure-args (kvector-getter <gnu-build> configure-args:))
	($triple         (kvector-getter <gnu-build> triple:))
	(gnu?            (kvector-predicate <gnu-build>)))
    (lambda (v)
      `(,@($pre-configure v)
	,@(exports->script ($exports v))
	,@(if ($out-of-tree v)
	      `((if ((mkdir -p "distill-builddir")))
		(cd "distill-builddir")
		(if (("../configure"
		      ,@($configure-args v)))))
	      `((if ((./configure ,@($configure-args v))))))
	(if ((make ,@($make-flags v))))
	(if ((make ,@($install-flags v))))
	,@($post-install v)
	(foreground ((rm -rf /out/usr/share/man /out/usr/share/info)))
	(foreground ((find /out -type f -name "*.la" -delete)))
	,@(strip-binaries-script ($triple v))))))

(: <ska-build> vector)
(define <ska-build>
  (make-kvector-type
   triple:
   exports:
   configure-args:
   make-args:))

(: $ska-build (vector --> vector))
(define $ska-build
  (let ((%make (kvector-constructor <ska-build>)))
    (lambda (conf)
      (let ((sysroot ($sysroot conf)))
	(%make
	 triple:  ($triple conf)
	 exports: (list ($cc-env conf))
	 configure-args:
	 `(--target ,($triple conf) --prefix=/ --libdir=/usr/lib
		    ,(conc "--with-include=" (filepath-join sysroot "/include"))
		    ,(conc "--with-include=" (filepath-join sysroot "/usr/include"))
		    ,(conc "--with-lib=" (filepath-join sysroot "/lib"))
		    ,(conc "--with-lib=" (filepath-join sysroot "/usr/lib"))
		    --disable-shared --enable-static)
	 make-args: (kvargs ($make-overrides conf)))))))

(: ska-recipe (vector --> list))
(define ska-recipe
  (let (($exports        (kvector-getter <ska-build> exports:))
	($configure-args (kvector-getter <ska-build> configure-args:))
	($make-args      (kvector-getter <ska-build> make-args:))
	($triple         (kvector-getter <ska-build> triple:))
	(ska?            (kvector-predicate <ska-build>)))
    (lambda (bld)
      (unless (ska? bld)
	      (error "ska-recipe given a non-<ska-build> object:" bld))
      `(,@(exports->script ($exports bld))
	;; don't let the configure script override our CFLAGS selections
	(if ((sed "-i" -e "/^tryflag.*-fno-stack/d" -e "s/^CFLAGS=.*$/CFLAGS=/g" configure)))
	(if ((./configure ,@($configure-args bld))))
	(if ((make ,@($make-args bld))))
	(if ((make DESTDIR=/out install)))
	,@(strip-binaries-script ($triple bld))))))

