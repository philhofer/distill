;; *this-machine* is the architecture
;; for which the currently-running program
;; was compiled
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
;; for writing package definitions.
;;
;; Typically, callers should prefer
;; higher-level package constructors
;; like cmmi-package.
;;
;; The 'label' argument should indicate
;; the label for the package to be defined.
;;
;; The 'build' argument should specify
;; the build script to run. (The build script
;; will be expanded against the configuration.)
;;
;; The following keyword arguments are accepted:
;;
;;  - src: the list of source artifacts
;;  - cross: the list of packages that should be
;;  - expanded against the host architecture
;;  but be unpacked into /
;;  - tools: the list of packages that should
;;  be expanded against the build architecture
;;  and unpacked into /
;;  - inputs: the list of packages that should
;;  be expanded against the host architecture
;;  and unpacked in its sysroot
;;  - patches: a list of patch files (as artifacts)
;;  to apply
;;  - dir: the directory to switch into for building
;;  - env: an alist of environment variables to set
;;  - raw-output: either `#f` or the name of the output
;;  file that should be treated as an output image
;;
;; If raw-output is not specified, then the package-template
;; automatically adds tar and zstd to the list of tools
;; and produces the final result at "/result.tar.zst" by
;; archiving the directory "/out".
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
         (raw-output #f))
  (lambda (host)
    (let* ((name      (conc label "-" ($arch host)))
           (bld       ($build host))
           (strap     (%bootstrap bld))
           (ex        (%current-expander))
           (boot      ($boot-tools bld))
           (output    (or raw-output "/result.tar.zst"))
           (build     (if raw-output build `(if ,(if (list? build) build (list build))
                                                ;; note: the sandbox sets LC_ALL=C.UTF-8,
                                                ;; so the sort order should be stable here:
                                                tar -Izstd -cf "/result.tar.zst"
                                                --format=ustar --sort=name
                                                --owner=0 --group=0 "--mtime=@0"
                                                --numeric-owner -C /out ".")))
           (tools    (if raw-output
                         ;; just inject execline for raw output:
                         (append (%execline bld) tools)
                         ;; inject execline *and* archival tools:
                         (append (%execline bld) (%archive-tools bld) tools)))
           ;; tools are built with the 'build' config,
           ;; unless they are part of bootcc or boottar,
           ;; in which case they are built with the boostrap config
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
                             link: link)))))
           (flatmap   (lambda (proc lst)
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
                                             (else (cons h out)))))))))))
           (buildfile (lambda (conf dir env pathces build)
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
                                (else (error "expected build; got" build))))))))))))
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
                    (map ->tool (flatmap exp/build
                                         (flatten src tools (map uncross cross))))
                    (map ->input (flatmap exp/host inputs))))))))))

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

;; subpackage takes a package definition
;; and returns a subset of the output artifacts
;; of that package.
;;
;; The prefix (string) argument indicates what prefix
;; should be applied to the package label to
;; produce the new output package label.
;;
;; The sub argument indicates the package to be
;; used as the base package.
;;
;; The remaining arguments are directories
;; (relative to ".") that should be included.
(: subpackage (string * #!rest string -> (vector -> vector)))
(define (subpackage prefix sub . dirs)
  (define the-package
    (package-template
     label: "overwrite-me"
     build: `(pipeline (tar -cf -
                            --ignore-failed-read ;; don't blow up if a glob doesn't match
                            --format=ustar --sort=name
                            --owner=0 --group=0 "--mtime=@0"
                            --numeric-owner -C ,$sysroot ,@dirs)
                       zstd - -o "/result.tar.zst")
     tools:  (list %archive-tools) ; explicit due to raw-output
     inputs: (list sub)
     no-libc: #t
     raw-output: "/result.tar.zst"))
  (lambda (conf)
    ;; during expansion, fiddle with the
    ;; name of the returned package just a bit:
    (let* ((child (configure sub conf))
           (name  (string-append prefix (plan-name child)))
           (self  (configure the-package conf)))
      (kupdate self name: name))))

;; libs wraps a package and yields
;; only the parts of its outputs that
;; are in conventional locations for C library files:
;;  - /usr/lib
;;  - /lib
;;  - /usr/include
;;  - /include
;;  - /usr/share
(define (libs pkg)
  (subpackage "lib-" pkg
              "./usr/lib/" "./lib/"
              "./usr/include/" "./include/"
              "./usr/share/"))

;; binaries produces a subpackage
;; by matching common binary directories:
;;  - /usr/bin
;;  - /bin
;;  - /usr/sbin
;;  - /sbin
;;  - /usr/libexec
;;  - /libexec
;;  - /usr/share
;;  - /share
(define (binaries pkg)
  (subpackage "bin-" pkg
              "./usr/bin/" "./bin/"
              "./usr/sbin/" "./sbin/"
              "./usr/libexec/" "./libexec/"
              "./usr/share/" "./share/"))

;; url-translate takes a url string in urlfmt
;; and a name and version string to substitute
;; into the literal substrings "$name" and "$version"
;; in urlfmt
(define (url-translate urlfmt name version)
  (and urlfmt
       (string-translate* urlfmt `(("$name" . ,name)
                                   ("$version" . ,version)))))

;; cc-package is a template for C/C++ packages
;;
;; The cc-package template picks suitable defaults
;; for the package source, inputs (libc), tools (toolchain)
;; and build directory; the additional keyword arguments
;; can be used to augment that package template with
;; more libraries and tools.
;;
;; The primary formals for cc-package are as follows:
;;
;; - `name` is the name of the package (a string)
;; - `version` is the version of the package (also a string)
;; - `urlfmt` is the URL at which the source tarball for
;; this package lives, optionally with "$name" and "$version"
;; in the url string that will be automatically substituted;
;; urlfmt can be `#f` if `hash-or-art` present an artifact object instead
;; - `hash-or-art` is either a string indicating the expected
;; hash of the output tarball, or an artifact value containing
;; the source code
;;
;; The 'build:' keyword argument is mandatory; cc-package
;; does not provide a default build script.
;;
;; The following additional keyword arguments are accepted:
;;
;;  - dir: overrides the build directory; by default
;;  `(string-append name "-" version)` is used
;;  - build: the build script for building the package
;;  - no-libc: can be set to `#t` to disable pulling in the config's base libc
;;  - use-native-cc: can be set to `#t` to pull in a C toolchain
;;  that can be used for building binaries that run on the build architecture
;;
;; The following additional keyword arguments
;; are accepted and passed to the inner call to `package-template`,
;; along with the requisite items for C/C++ compilation:
;;
;;  - tools:
;;  - cross:
;;  - patches:
;;  - env:
;;
(define (cc-package
         name version urlfmt hash-or-art
         #!key
         (dir #f) ;; directory override
         (build #f)
         (no-libc #f)  ;; do not bring in a libc implicitly
         (use-native-cc #f)
         (raw-output #f)
         (patches '()) ;; patches to apply
         (env '())     ;; extra environment
         (libs '())    ;; extra libraries beyond libc
         (tools '())   ;; extra tools beyond a C toolchain
         (cross '())
         (extra-src '()))
  (let* ((url (url-translate urlfmt name version))
         (src (cond
               (url (remote-archive url hash-or-art))
               ((artifact? hash-or-art) hash-or-art)
               (else (error "invalid hash argument to cc-package" hash-or-art))))
         ($native-libc  (o cc-toolchain-libc $native-toolchain))
         ($native-tools (o cc-toolchain-tools $native-toolchain)))
    (package-template
     raw-output: raw-output
     patches: patches
     label:   (conc name "-" version)
     src:     (cons src (append patches extra-src))
     env:     env
     dir:     (or dir (conc name "-" version))
     cross:   (cons $cc-tools cross)
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
         (dir #f) ;; directory override
         (patches '()) ;; patches to apply
         (env '())     ;; extra environment

         ;; override-configure, if not #f, takes
         ;; precedence over the default configure options;
         ;; otherwise (append default extra-configure) is used
         (extra-configure '())
         (override-configure #f)

         (override-make #f)
         (override-install #f)
         (out-of-tree #f)

         (libs '()) ;; extra libraries beyond libc
         (tools '()) ;; extra tools beyond a C toolchain
         (cross '())
         (extra-src '()) ;; extra source
         (prepare #f)    ;; a place to put sed chicanery, etc.
         (cleanup #f)    ;; a place to put extra install tweaking
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
     libs: libs
     tools: tools
     cross: cross
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
  ;; tools for archiving results (need tar and zstd)
  (%archive-tools archive:      '() list?)
  ;; tools supplying execline (required everywhere)
  (%execline      execline:     '() list?)
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

(define $boot-tools
  (lambda (conf)
    (append (%archive-tools conf) (%execline conf) ($cc-tools conf))))

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
   `(foreground
     ;; note: strip(1) will complain if it sees
     ;; a shell script here instead of an ELF binary,
     ;; but that's fine; just ignore those errors...
     (find /out -type f -perm -o+x -exec ,(elconc $triple "-strip") "{}" ";"))))

;; bind locates a file relative to the current working directory,
;; the current install directory, or the current install's lib directory,
;; and produces an artifact that appears at 'therepath' with the same
;; permissions as the file has locally
;;
;; if herepath is an artifact, bind returns
;; a new artifact with the target path updated
;; to point to therepath
;;
;; TODO: handle directories
(define (bind herepath therepath)
  (or (update-path herepath therepath)
      (let ((f (or
                (file-exists? herepath)
                (let ((ep (executable-pathname)))
                  (or (file-exists? (filepath-join ep herepath))
                      (file-exists? (filepath-join ep "../../lib/distill/" herepath))
                      (error "couldn't find file" herepath))))))
        (overlay f therepath))))

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
     ((list? x)      (join-with " " (map rhs x)))
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
