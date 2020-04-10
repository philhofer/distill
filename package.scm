(: *this-machine* symbol)
(define *this-machine*
  (cond-expand
    (x86-64 'x86_64)
    (arm64  'aarch64)
    ;; TODO: additional verification that this is armv7-a
    (arm    'armv7)
    ((and ppc64 little-endian) 'ppc64le)
    ((and ppc64 big-endian) 'ppc64)))

(: <recipe> vector)
(define <recipe>
  (make-kvector-type
    script:))

(: make-recipe (#!rest * -> vector))
(define make-recipe
  (kvector-constructor
    <recipe>
    script: #f  list?))

(: recipe? (* --> boolean))
(define recipe? (kvector-predicate <recipe>))
(: recipe-script (vector -> list))
(define recipe-script (kvector-getter <recipe> script:))

(: <package> vector)
(define <package>
  (make-kvector-type
    label:
    src:
    tools:
    inputs:
    prebuilt:
    build:
    raw-output:))

(: package? (* -> boolean))
(define package? (kvector-predicate <package>))

(: make-package (#!rest * -> vector))
(define make-package
  (kvector-constructor
    <package>
    label:    #f  string?
    src:      '() (or/c artifact? (list-of artifact?))
    tools:    '() (list-of (or/c procedure? artifact?))
    inputs:   '() (list-of (or/c procedure? artifact?))
    prebuilt: #f  (or/c false/c artifact?)
    build:    #f  recipe?
    raw-output: #f (or/c false/c string?)))

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
(: package-build (vector --> vector))
(define package-build (kvector-getter <package> build:))
(: package-raw-output (vector --> (or string false)))
(define package-raw-output (kvector-getter <package> raw-output:))

(: <config> vector)
(define <config>
  (make-kvector-type
    arch:
    configure-flags:
    sysroot:
    triple:
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

(: config? (* -> boolean))
(define config? (kvector-predicate <config>))

(: $arch (vector --> symbol))
(define $arch    (kvector-getter <config> arch:))
(: $triple (vector --> string))
(define $triple  (kvector-getter <config> triple:))
(: $sysroot (vector --> string))
(define $sysroot (kvector-getter <config> sysroot:))
(: $configure-flags (vector --> list))
(define $configure-flags (kvector-getter <config> configure-flags:))

(: $CC (vector --> list))
(define $CC (kvector-getter <config> CC:))
(: $CXX (vector --> list))
(define $CXX (kvector-getter <config> CXX:))
(: $AR (vector --> string))
(define $AR (kvector-getter <config> AR:))
(: $NM (vector --> string))
(define $NM (kvector-getter <config> NM:))
(: $LD (vector --> list))
(define $LD (kvector-getter <config> LD:))
(: $ARFLAGS (vector --> *))
(define $ARFLAGS (kvector-getter <config> ARFLAGS:))
(: $CFLAGS (vector --> list))
(define $CFLAGS (kvector-getter <config> CFLAGS:))
(: $CXXFLAGS (vector --> list))
(define $CXXFLAGS (kvector-getter <config> CXXFLAGS:))
(: $LDFLAGS (vector --> list))
(define $LDFLAGS (kvector-getter <config> LDFLAGS:))
(: $RANLIB (vector --> string))
(define $RANLIB  (kvector-getter <config> RANLIB:))
(: $READELF (vector --> string))
(define $READELF (kvector-getter <config> READELF:))

(: build-triple symbol)
(define build-triple
  (string->symbol
    (conc *this-machine* "-linux-musl")))

(define (+cross conf normal extra)
  (if (eq? ($arch conf) *this-machine*)
    normal
    (append extra normal)))

(: %make-config (#!rest * -> vector))
(define %make-config (kvector-constructor <config>))

(define (config* #!key
                 (arch     *this-machine*)
                 (CFLAGS   '(-pipe -Os -fstack-protector-strong))
                 (CXXFLAGS '(-pipe -Os -fstack-protector-strong))
                 (LDFLAGS  '())
                 (sysroot  #f))
  (let* ((triple  (conc arch "-linux-musl"))
         (sysroot (or sysroot
                      (filepath-join "/sysroot/" triple)))
         (sysflag (conc "--sysroot=" sysroot)))
    (%make-config
      arch:    arch
      triple:  triple
      sysroot: sysroot
      configure-flags: `(--disable-shared --disable-nls
                                          --enable-static
                                          --enable-pie
                                          --with-pic
                                          --prefix=/usr
                                          --sysconfdir=/etc
                                          --build ,build-triple
                                          --host ,triple)
      CC:      (list (conc triple "-gcc") sysflag)
      CXX:     (list (conc triple "-g++") sysflag)
      LD:      (list (conc triple "-ld") sysflag)
      AS:      (conc triple "-as")
      CBUILD:  build-triple
      CHOST:   triple
      CFLAGS:  (append
                 '(-fPIE -static-pie) CFLAGS)
      LDFLAGS: (append
                 '(-static-pie) LDFLAGS)
      CXXFLAGS: (append
                  '(-fPIE -static-pie) CXXFLAGS)
      AR:      (conc triple "-ar")
      ARFLAGS: '-Dcr
      NM:      (conc triple "-nm")
      RANLIB:  (conc triple "-ranlib")
      STRIP:   (conc triple "-strip")
      READELF: (conc triple "-readelf")
      OBJCOPY: (conc triple "-objcopy"))))

;; $cc-env is the subset of <config>
;; that is relevant to ordinary C compilation
;; (see also $make-overrides)
(define $cc-env
  (memoize-one-eq
    (subvector-constructor
      <config>
      CC: AR: LD: AS: CXX: CBUILD: CHOST: CFLAGS: CXXFLAGS: LDFLAGS:)))

;; $make-overrides is the subset of <config>
;; that is supplied as k=v arguments to invocations
;; of $MAKE (in order to override assignments in the Makefile)
(define $make-overrides
  (memoize-one-eq
    (subvector-constructor
      <config>
      RANLIB: STRIP: NM: READELF: OBJCOPY: AR: ARFLAGS:)))

;; native-config is the config used for packages
;; that need to build binaries for the *build*
;; system in order to complete a cross-build
;; (for example, Kbuild builds itself before
;; performing the "real" build)
;;
;; this differs in a subtle but important way
;; from ordinary build configs: sysroot is actually "/"
(define native-config
  (config*
    arch:     *this-machine*
    sysroot:  "/"
    CFLAGS:   '(-pipe -O2)
    CXXFLAGS: '(-pipe -O2)))

;; cc-env/build produces a C environment
;; by transforming the usual CC, LD, etc.
;; variables using (frob-kw key) for each keyword
(: cc-env/build ((keyword -> keyword) -> vector))
(define cc-env/build
  (let ((sub (subvector-constructor
               <config>
               ;; a conservative guess at what sort of configuration
               ;; one would need for native tool builds; GCC is the worst
               ;; offender here: it needs very specific stuff like RANLIB, etc.
               CC: CFLAGS: LD: LDFLAGS: CXX: CXXFLAGS: AS:
               RANLIB: STRIP: NM: READELF: OBJCOPY: AR: ARFLAGS:)))
    (lambda (frob-kw)
      (let ((folder (lambda (kw arg lst)
                      (cons
                        (frob-kw kw)
                        (cons arg lst)))))
        (list->kvector
          (kvector-foldl
            (sub native-config)
            folder
            '()))))))

;; cc-env/for-build is a C environment
;; with CC_FOR_BUILD, CFLAGS_FOR_BUILD, etc.
;; (this is typical for autotools-based configure scripts)
(define cc-env/for-build
  (cc-env/build (lambda (kw)
                  (string->keyword
                    (string-append
                      (keyword->string kw)
                      "_FOR_BUILD")))))

;; cc-env/for-kbuild is a C environment
;; with HOSTCC, HOSTCFLAGS, etc.
;; (this is typical for Kbuild-based build systems)
(define cc-env/for-kbuild
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

(define (default-config arch)
  (config* arch: arch))

;; build-config is the configuration
;; for artifacts produced for *this* machine (tools, etc)
(define build-config
  (make-parameter (default-config *this-machine*)))

;; config->builder takes a configuration (as an alist or conf-lambda)
;; and returns a function that builds the package
;; and yields its output artifact
(define (config->builder env)
  (let* ((host  (conform config? env))
         (build (build-config))
         (->pln (lambda (pl)
                  (package->plan pl build host)))
         (->out (lambda (pln)
                  (if (artifact? pln) pln (plan-outputs pln)))))
    (lambda args
      (let ((plans (map ->pln args)))
        (unless (null? plans)
          (build-graph! plans))
        (map ->out plans)))))

;; package->plan is the low-level package expansion code;
;; it recursively simplifies packges into plans, taking care
;; to only expand packges once for each (build, host) configuration tuple
(define package->plan
  (memoize-lambda
    (pkg-proc build host)
    (let ((pkg (pkg-proc host)))
      (or (package-prebuilt pkg)
          (let* ((->tool (lambda (tool)
                           (if (artifact? tool)
                             tool
                             (package->plan tool build build))))
                 (->input (lambda (input)
                            (if (artifact? input)
                              input
                              (package->plan input build host))))
                 (tools  (package-tools pkg))
                 (inputs (package-inputs pkg))
                 (recipe (package-build pkg)))
            (make-plan
              raw-output: (package-raw-output pkg)
              name:   (package-label pkg)
              inputs: (list
                        (cons "/" (flatten
                                    (recipe-buildfile recipe)
                                    (package-src pkg) (map ->tool tools)))
                        ;; build headers+libraries live here
                        (cons ($sysroot host) (map ->input inputs)))))))))

(: recipe-buildfile (vector -> vector))
(define (recipe-buildfile r)
  (interned
    "/build" #x744
    (lambda ()
      (write-exexpr (recipe-script r)))))

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
          configure-args: ($configure-flags conf))))))

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
    (lambda (dir v)
      (unless (gnu? v)
        (error "gnu-recipe called on non-<gnu-build> object:" v))
      (make-recipe
        script: `((cd ,dir)
                  ,@($pre-configure v)
                  ,@(exports->script ($exports v))
                  ,@(if ($out-of-tree v)
                      `((if ((mkdir -p "/builddir")))
                        (cd "/builddir")
                        (if ((,(conc "/" dir "/configure")
                               ,@($configure-args v)))))
                      `((if ((./configure ,@($configure-args v))))))
                  (if ((make ,@($make-flags v))))
                  (if ((make ,@($install-flags v))))
                  ,@($post-install v)
                  (foreground ((rm -rf /out/usr/share/man)))
                  (foreground ((rm -rf /out/usr/share/info)))
                  (foreground ((find /out -type f -name "*.la" -delete)))
                  ,@(strip-binaries-script ($triple v)))))))

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

(: ska-recipe (string vector --> vector))
(define ska-recipe
  (let (($exports        (kvector-getter <ska-build> exports:))
        ($configure-args (kvector-getter <ska-build> configure-args:))
        ($make-args      (kvector-getter <ska-build> make-args:))
        ($triple         (kvector-getter <ska-build> triple:))
        (ska?            (kvector-predicate <ska-build>)))
    (lambda (dir bld)
      (unless (ska? bld)
        (error "ska-recipe given a non-<ska-build> object:" bld))
      (make-recipe
        script:
        `((cd ,dir)
          ,@(exports->script ($exports bld))
          ;; don't let the configure script override our CFLAGS selections
          (if ((sed "-i" -e "/^tryflag.*-fno-stack/d" -e "s/^CFLAGS=.*$/CFLAGS=/g" configure)))
          (if ((./configure ,@($configure-args bld))))
          (if ((make ,@($make-args bld))))
          (if ((make DESTDIR=/out install)))
          ,@(strip-binaries-script ($triple bld)))))))

