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
    env:
    script:))

(: make-recipe (#!rest * -> vector))
(define make-recipe
  (kvector-constructor
    <recipe>
    env:    '() (list-of pair?)
    script: #f  list?))

(: recipe? (* --> boolean))
(define recipe? (kvector-predicate <recipe>))
(: recipe-env (vector -> list))
(define recipe-env (kvector-getter <recipe> env:))
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
    raw-output:
    parallel:))

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
    raw-output: #f (or/c false/c string?)
    parallel:   #t (or/c (eq?/c 'very) boolean?)))

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
(: package-parallel (vector --> (or symbol boolean)))
(define package-parallel (kvector-getter <package> parallel:))

;; XXX this needs to stay in-sync with base
(define (sysroot conf)
  (string-append "/sysroot/"
                 (symbol->string (conf 'arch))
                 "-linux-musl"))

;; build-config is the configuration
;; for artifacts produced for *this* machine (tools, etc)
(define build-config
  (make-parameter
    (table->proc (table `((arch . ,*this-machine*)
                          (CFLAGS -pipe -fstack-protector-strong -Os))))))

(define (default-config arch)
  (table->proc (table `((arch . ,arch)
                        (CFLAGS -pipe -fstack-protector-strong -Os)))))

(define (%check-conf conf)
  (or (memq (conf 'arch) '(x86_64 aarch64 ppc64 ppc64le armv7))
      (error "bad config (unrecognized arch)" (conf 'arch))))

;; config->builder takes a configuration (as an alist or conf-lambda)
;; and returns a function that builds the package
;; and yields its output artifact
(define (config->builder env)
  (let* ((host  (if (pair? env)
                  (table->proc (table env))
                  (begin (%check-conf env) env)))
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
              parallel: (package-parallel pkg)
              name:   (package-label pkg)
              inputs: (list
                        (cons "/" (flatten
                                    (recipe-envfile recipe)
                                    (recipe-buildfile recipe)
                                    (package-src pkg) (map ->tool tools)))
                        ;; build headers+libraries live here
                        (cons (sysroot host) (map ->input inputs)))))))))

(define (package->stages conf root)
  (let ((plan (package->plan root (build-config) conf)))
    (compute-stages plan)))

;; default environment for recipes
(define *default-env*
  '((PATH . "/bin:/sbin:/usr/bin:/usr/sbin:/usr/local/bin")
    (LC_ALL . "C.UTF-8")
    (SOURCE_DATE_EPOCH . "0")))

(: real-env (vector --> (list-of pair)))
(define (real-env r)
  (append *default-env* (recipe-env r)))

(: recipe-envfile (vector --> vector))
(define (recipe-envfile r)
  (let ((str (call-with-output-string
               (lambda (oport)
                 (for-each
                   (lambda (pr)
                     (display (car pr) oport)
                     (display "=" oport)
                     (display (cdr pr) oport)
                     (newline oport))
                   (real-env r))))))
    (interned
      "/env" #o644 str)))

(: recipe-buildfile (vector -> vector))
(define (recipe-buildfile r)
  (let ((str (with-output-to-string
               (lambda () (write-exexpr (recipe-script r))))))
    (interned
      "/build" #x744 str)))
