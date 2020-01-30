(: *this-machine* symbol)
(define *this-machine*
  (cond-expand
    (x86-64 'x86_64)
    (arm64  'aarch64)
    ;; TODO: additional verification that this is armv7-a
    (arm    'armv7)
    ((and ppc64 little-endian) 'ppc64le)
    ((and ppc64 big-endian) 'ppc64)))

(define-type artifact (vector vector string *))
(define-type conf-lambda (symbol -> *))
(define-type package-lambda (conf-lambda -> (struct package)))

;; package: a "portable" intermediate representation of a package
;; that is converted into a plan by combining it with the configuration
(defstruct package
  (label : string)                    ;; human-readable name
  (src : (or artifact (list-of artifact))) ;; where to get the package source
  (tools : (list-of package-lambda))  ;; build tools (built for host)
  (inputs : (list-of package-lambda)) ;; build dependencies (built for target)
  (prebuilt : (or artifact false))    ;; bootstrap replacement binary
  (build : (struct recipe))           ;; build script (see execline*)
  ((parallel #t) : boolean))

;; XXX this needs to stay in-sync with base
(: sysroot (conf-lambda --> string))
(define (sysroot conf)
  (string-append "/sysroot/"
                 (symbol->string (conf 'arch))
                 "-linux-musl"))

;; build-config is the configuration
;; for artifacts produced for *this* machine (tools, etc)
(: build-config (-> conf-lambda))
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
(: config->builder ((or (list-of pair) conf-lambda) -> (#!rest package-lambda -> artifact)))
(define (config->builder env)
  (let* ((host  (if (pair? env)
                  (table->proc (table env))
                  (begin (%check-conf env) env)))
         (build (build-config))
         (->pln (lambda (pl)
                  (package->plan pl build host))))
    (lambda args
      (let ((plans (map ->pln args)))
        (unless (null? plans)
          (build-graph! plans))
        (map plan-outputs plans)))))

;; package->plan is the low-level package expansion code;
;; it recursively simplifies packges into plans, taking care
;; to only expand packges once for each (build, host) configuration tuple
(: package->plan (package-lambda conf-lambda conf-lambda -> (struct plan)))
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
              parallel: (package-parallel pkg)
              name:   (package-label pkg)
              recipe: (package-build pkg)
              inputs: (list
                        (cons "/" (flatten
                                    (recipe-envfile recipe)
                                    (recipe-buildfile recipe)
                                    (package-src pkg) (map ->tool tools)))
                        ;; build headers+libraries live here
                        (cons (sysroot host) (map ->input inputs)))))))))

(: package->stages (conf-lambda package-lambda -> vector))
(define (package->stages conf root)
  (let ((plan (package->plan root (build-config) conf)))
    (compute-stages plan)))

;; default environment for recipes
(define *default-env*
  '((PATH . "/bin:/sbin:/usr/bin:/usr/sbin:/usr/local/bin")
    (LC_ALL . "C.UTF-8")
    (SOURCE_DATE_EPOCH . "0")))

(defstruct recipe
  ((env '()) : (list-of pair))
  (script : list))

(: real-env ((struct recipe) --> (list-of pair)))
(define (real-env r)
  (append *default-env* (recipe-env r)))

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

(define (recipe-buildfile r)
  (let ((str (with-output-to-string
               (lambda () (write-exexpr (recipe-script r))))))
    (interned
      "/build" #x744 str)))
