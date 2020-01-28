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
  (build : (struct recipe)))          ;; build script (see execline*)

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
        (build-graph! plans)
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
                 (inputs (package-inputs pkg)))
            (make-plan
              name:   (package-label pkg)
              recipe: (package-build pkg)
              inputs: (list
                        ;; build tools live here
                        (cons "/" (flatten (package-src pkg) (map ->tool tools)))
                        ;; build headers+libraries live here
                        (cons (sysroot host) (map ->input inputs)))))))))

(: package->stages (conf-lambda package-lambda -> vector))
(define (package->stages conf root)
  (let ((plan (package->plan root (build-config) conf)))
    (compute-stages plan)))

;; write-digraph displays the dependency graph for a list of packages
;; in a format that can be used by the dot(1) tool
(: write-digraph (conf-lambda conf-lambda #!rest package-lambda -> *))
(define (write-digraph build host . pkgs)
  (let* ((plans (map (cut package->plan <> build host) pkgs))
         (ht    (make-hash-table))
         (label (lambda (p)
                  (string-append (plan-name p) " " (short-hash (plan-hash p)))))
         (outhash (lambda (p)
                    (short-hash (artifact-hash (plan-outputs p))))))
    (display "digraph packages {\n")
    (for-each
      (lambda (p)
        (plan-dfs
          (lambda (p)
            (when (and (plan? p)
                       (not (hash-table-ref/default ht p #f)))
              (hash-table-set! ht p #t)
              (write (label p))
              (display " -> ")
              (write (outhash p))
              (display ";\n")
              (for-each
                (lambda (in)
                  (cond
                    ((plan? in)
                     (write (outhash in)))
                    ((artifact? in)
                     (case (vector-ref (artifact-format in) 0)
                       ;; try to produce a moderately informative textual representation;
                       ;; track labels so that bootstrap packages create the appropriate
                       ;; circular references
                       ((archive)      (write (or (artifact-extra in)
                                                  (short-hash (artifact-hash in)))))
                       ((file symlink) (write (vector-ref (artifact-format in) 1)))
                       (else           (write (short-hash (artifact-hash in)))))))
                  (display " -> ")
                  (write (label p))
                  (display ";\n"))
                (apply append (map cdr (plan-inputs p))))))
          p))
      plans)
    (display "}\n")))

