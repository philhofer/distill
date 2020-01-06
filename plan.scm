

;; artifacts are just vectors
;;
;; note that artifact-extra must not contain
;; data that changes the filesystem representation
;; of the artifact (it is ignored for plan hashing purposes)
(define-type artifact (vector vector string *))
(: %artifact (forall (a) (vector string a --> (vector vector string a))))
(define (%artifact format hash extra)
  (vector format hash extra))

(: artifact-hash (artifact --> string))

(define (artifact-format v) (vector-ref v 0))
(define (artifact-hash v)   (vector-ref v 1))
(define (artifact-extra v)  (vector-ref v 2))
(define (artifact? v) (and (vector? v) (= (vector-length v) 3)))

;; artifact-repr converts an artifact to its
;; external representation (which omits metadata
;; that doesn't change the contents of the artifact)
(: artifact-repr (forall (a) (string (vector a string *) --> (vector string a string))))
(define (artifact-repr root v)
  (vector root (artifact-format v) (artifact-hash v)))

;; guess the format of a remote source bundle
(define (impute-format src)
  (unless (string-prefix? "https://" src)
    (error "source spec doesn't begin with https://:" src))
  (let loop ((suff '((".tar.xz" . tar.xz)
		     (".tar.gz" . tar.gz)
		     (".tgz"    . tar.gz)
		     (".tar.bz" . tar.bz)
		     (".tar.bz2". tar.bz)
		     (".tar"    . tar))))
    (cond
      ((null? suff) 'unknown)
      ((string-suffix? (caar suff) src) (cdar suff))
      (else (loop (cdr suff))))))

(: remote-archive (string string --> artifact))
(define (remote-archive src hash)
  (%artifact
    `#(archive ,(impute-format src))
    hash
    src))

(: local-archive (symbol string --> artifact))
(define (local-archive kind hash)
  (%artifact
   `#(archive ,kind)
    hash
    #f))

(: %local-file (string integer string --> artifact))
(define (%local-file abspath mode hash)
  (%artifact
    `#(file ,abspath ,mode)
    hash
    #f))

(: interned (string integer string --> artifact))
(define (interned abspath mode contents)
  (%artifact
    `#(file ,abspath ,mode)
    (hash-string contents)
    contents))

(: interned-symlink (string string --> vector))
(define (interned-symlink abspath lnk)
  (%artifact
    `#(symlink ,abspath ,lnk)
    (hash-string lnk)
    #f))

;; by default, dump stuff into these directories
;; TODO: inherit these from the environment
(define plan-dir (make-parameter "./plans"))
(define artifact-dir (make-parameter "./artifacts"))

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

(define-type conf-lambda (symbol -> *))
(define-type package-lambda (conf-lambda -> (struct package)))

;; package: a "portable" intermediate representation of a package
;; that is converted into a plan by combining it with the configuration
(defstruct package
	   (label : string)                    ;; human-readable name
	   (src : (or artifact (list-of artifact))) ;; where to get the package source
	   (tools : (list-of package-lambda))  ;; build tools (built for host)
	   (inputs : (list-of package-lambda)) ;; build dependencies (built for target)
	   (build : (struct recipe))           ;; build script (see execline*)
	   ((overlay '()) : (list-of artifact))) ;; additional files to include (patches, etc.)

;; plan is the lowest-level representation of a "package"
;; or other build step; it simply connects itself to other
;; inputs plus a recipe for producing the output
;;
;; essentially, packages are "compiled" as
;; package-lambda -> struct package -> struct plan
;;
;; or, viewed as code:
;; (%package->plan host-conf target-conf (package-lambda target-conf))
;;
;; DO NOT USE SETTERS on this structure; you will invalidate
;; the saved-* values that are used to reduce the amount
;; of dependency graph traversal necessary to produce plan input and output hashes
(define-type plan-input-type (or (struct plan) vector))
(defstruct plan
	   (name : string)
	   ;; recipe is the code+environment to be executed
	   (recipe : (struct recipe))
	   ;; inputs are the set of filesystem inputs
	   ;; represented as an alist of root directories
	   ;; and contents to be extracted relative to those roots
	   ;; (see 'unpack')
	   (inputs : (list-of
		       (pair string (list-of plan-input-type))))
	   ((saved-hash #f) : (or false string))
	   ((saved-output #f) : (or false vector)))

(define (%plan name recipe inputs)
  (make-plan
    #:name       name
    #:recipe     recipe
    #:inputs     inputs
    #:saved-hash (%plan-hash inputs recipe)))

(define-syntax require
  (syntax-rules ()
    ((_ pred? obj)
     (if (pred? obj)
	 obj
	 (error "object doesn't satisfy predicate" (quote pred?) obj)))
    ((_ pred? obj msg rest* ...)
     (if (pred? obj)
	 obj
	 (error msg rest* ...)))))

;; note here that 'host' is the config for the
;; machine running the build, and 'target' is
;; the config for the machines that consumes
;; build outputs (in GNU 'configure' terminology,
;; those would be 'build' and 'host,' respectively)
(: %package->plan (package-lambda conf-lambda conf-lambda -> (struct plan)))
(define-memoized (%package->plan pkg-proc host target)
  (let ((pkg (pkg-proc target)))
    (%plan
      (require ok-plan-name? (package-label pkg))
      (package-build pkg)
      (list
	(cons "/" (flatten (package-src pkg)
			     (package-overlay pkg)
			     (map (cut %package->plan <> host host)
				  (package-tools pkg))))
	(cons "/sysroot" (map (cut %package->plan <> host target)
			      (package-inputs pkg)))))))

;; since we use plans in filepaths, disallow
;; plans that begin with '.' or contain '/' or whitespace, etc.
(: ok-plan-name? (string --> boolean))
(define (ok-plan-name? str)
  (and (not (eqv? (string-ref str 0) #\.))
       (not (string-any
	      (lambda (c)
		(memv c '(#\/ #\space #\newline)))
	      str))))

(: write-env ((list-of pair) port -> void))
(define (write-env env prt)
  (for-each
    (lambda (p)
      (display (car p) prt)
      (display "=" prt)
      (display (cdr p) prt)
      (newline prt))
    env))

;; not defined in R7RS, but trivial enough to implement
(: call-with-output-string ((output-port -> *) -> string))
(define (call-with-output-string proc)
  (let ((p (open-output-string)))
    (proc p)
    (let ((s (get-output-string p)))
      (close-output-port p)
      s)))

(define (foldl1 proc seed lst)
  (let loop ((state seed)
	     (lst   lst))
    (if (null? lst)
	state
	(loop (proc state (car lst)) (cdr lst)))))

;; plan-hash computes the canonical hash for
;; a set of plan inputs and a recipe
(: %plan-hash ((list-of
		 (pair string (list-of plan-input-type))) (struct recipe) --> (or string false)))
(define (%plan-hash inputs recipe)
  (call/cc
    (lambda (ret)

      ;; convert a plan input to an artifact, or abort #f
      (define (->artifact in)
	(if (plan? in)
	    (or (plan-outputs in)
		(ret #f))
	    in))

      ;; sort input artifacts by extraction directory,
      ;; then by content hash
      (define (art<? a b)
	(let ((aroot (vector-ref a 0))
	      (broot (vector-ref b 0))
	      (ahash (vector-ref a 2))
	      (bhash (vector-ref b 2)))
	  (or (string<? aroot broot)
	      (and (string=? aroot broot)
		   (string<? ahash bhash)))))

      ;; cons the canonical representation of 'inputs' at 'root' onto tail
      (define (cons*-reprs root inputs tail)
	(foldl1
	  (lambda (lst v)
	    (cons (artifact-repr root (->artifact v)) lst))
	  tail
	  inputs))

      ;; cons inputs onto tail
      ;;
      ;; produces an output like
      ;;  (#("/" #('file "/etc/hostname" #o644) "...<hash>...")
      ;;   #("/" #('archive (tar xz))           "...<hash>...") ...)
      (define (cons*-inputs in tail)
	(foldl1
	  (lambda (lst p)
	    (cons*-reprs (car p) (cdr p) lst))
	  tail
	  in))

      (let ((inputs (sort (cons*-inputs inputs '()) art<?)))
	(hash-string
	  (call-with-output-string
	    (lambda (out)
	      (for-each
		(lambda (in) (write in out) (newline out))
		inputs)
	      (write-env (real-env recipe) out)
	      (write-exexpr (recipe-script recipe) out))))))))

;; plan-hash returns the canonical hash of a plan,
;; or #f if any of its inputs have unknown outputs
(: plan-hash ((struct plan) -> (or false string)))
(define (plan-hash p)
  (or (plan-saved-hash p)
      (let ((h (%plan-hash
		 (plan-inputs p)
		 (plan-recipe p))))
	(plan-saved-hash-set! p h)
	h)))

;; generic DFS DAG-walking procedure
;;
;; call (proc node) on each node, and use
;; (get-edges node) to get a list of edges
;;
;; throws an error on cycles in the graph
;;
(: for-each-dag-dfs (forall (a) (a (a -> *) (a -> (list-of a)) -> *)))
(define (for-each-dag-dfs root proc get-edges)
  (define tbl (make-hash-table))
  (define (walked? x)
    (let ((c (hash-table-ref/default tbl x 0)))
      (when (= c 1)
	(error "cycle: " x))
      (not (= c 0))))
  (define (walk x)
    (unless (walked? x)
      (begin
	(hash-table-set! tbl x 1)
	(for-each walk (get-edges x))
	(hash-table-set! tbl x 2)
	(proc x))))
  (walk root))

;; plan-dfs calls (proc plan) on
;; each plan in a graph starting at 'root,'
;; ensuring that 'proc' is applied to all
;; inputs of a plan before being applied
;; to the plan itself
(: plan-dfs ((plan-input-type -> *) (struct plan) -> undefined))
(define (plan-dfs proc root)
  (for-each-dag-dfs root
		    proc
		    (lambda (x)
		      (if (plan? x)
			  (flatten (map cdr (plan-inputs x)))
			  '()))))

;; fork+exec, wait for the process to exit and check
;; that it exited successfully
(: run (string #!rest string -> undefined))
(define (run prog . args)
  (trace "run" (cons prog args))
  (let-values (((pid ok? status) (process-wait (process-run prog args))))
    (unless (and ok? (= status 0))
      (error "command failed:" (cons prog args)))))

;; run a command inside a new virtual root 'root'
;; which is a directory populated like a root filesystem
(: sandbox-run (string string #!rest string -> undefined))
(define (sandbox-run root prog . args)
  ;; TODO: I'm not pleased about leaning on a setuid program,
  ;; but there isn't a way to manipulate mount namespaces without
  ;; elevated privs...
  (apply
    run
    "bwrap"
    "--unshare-ipc"
    "--unshare-pid"
    "--unshare-uts"
    "--unshare-cgroup"
    "--hostname" "builder"
    "--bind" root "/"  ; rootfs containing host tools
    "--dir" "/dev"
    "--dir" "/proc"
    "--dir" "/tmp"
    "--dev" "/dev"
    "--proc" "/proc"
    "--tmpfs" "/tmp"
    "--"
    args))

(: fetch! (string string -> *))
(define (fetch! src hash)
  (let* ((dst (filepath-join (artifact-dir) hash))
	 (tmp (string-append dst ".tmp")))
    (info "fetching" hash "from" src)
    (run "wget" "-O" tmp src)
    (let ((h (hash-file tmp)))
      (if (string=? h hash)
	  (rename-file tmp dst #t)
	  (begin
	    (delete-file tmp)
	    (fatal "fetched artifact has the wrong hash:" h "not" hash))))))

;; plan-outputs-file is the file that stores the serialized
;; interned file information for a plan
(: plan-outputs-file ((struct plan) -> (or string false)))
(define (plan-outputs-file p)
  (and-let* ((h (plan-hash p)))
    (filepath-join (plan-dir) h "outputs.scm")))

;; write 'lst' as the set of plan outputs associated with 'p'
(: save-plan-outputs! ((struct plan) artifact -> *))
(define (save-plan-outputs! p ar)
  (let ((outfile (plan-outputs-file p)))
    (unless outfile
      (error "can't save outputs for plan:" (plan-name p)))
    (create-directory (dirname outfile))
    (with-output-to-file outfile (lambda () (write ar)))
    (plan-saved-output-set! p ar)))

;; determine the outputs (leaf) of the given plan,
;; or #f if the plan has never been built with its inputs
(: plan-outputs ((struct plan) -> (or artifact false)))
(define (plan-outputs p)
  (let ((saved (plan-saved-output p))
	(desc  (plan-outputs-file p)))
    (or saved
	(and desc (file-exists? desc)
	     (let ((vec (with-input-from-file desc read)))
	       (and (artifact? vec)
		    (begin
		      (plan-saved-output-set! p vec)
		      vec)))))))

;; unpack! installs a plan input into the
;; given destination directory
(: unpack! (artifact string -> *))
(define (unpack! i dst)

  (define (unpack-file dst abspath mode hash content)
    (let ((dstfile (filepath-join dst abspath)))
      (create-directory (dirname dstfile) #t)
      (if content
	  (with-output-to-file dstfile (lambda () (write-string content)))
	  (copy-file (filepath-join (artifact-dir) hash) dstfile))
      (set-file-permissions! dstfile mode)))

  (define (unpack-archive dst kind hash src)
    (let ((extract (case kind
		     ((tar.gz) "-zx")
		     ((tar.bz) "-jx")
		     ((tar.xz) "-Jx")
		     ((tar)    "-x")
		     (else (error "unknown archive kind" kind))))
	  (infile  (filepath-join (artifact-dir) hash)))
      (when (not (file-exists? infile))
	(unless src (fatal "archive" hash "doesn't exist and has no remote source"))
	(fetch! src hash))
      (run "tar" extract "-kf" infile "-C" dst)))

  (define (unpack-symlink dst abspath lnk)
    (create-symbolic-link lnk (filepath-join dst abspath)))

  (let ((format (artifact-format i))
	(hash   (artifact-hash i))
	(extra  (artifact-extra i)))
    (match format
      (#('file abspath mode)   (unpack-file dst abspath mode hash extra))
      (#('archive kind)        (unpack-archive dst kind hash extra))
      (#('symlink abspath lnk) (unpack-symlink dst abspath lnk))
      (else (error "unrecognized artifact format" format)))))

(define *envfile-path* "/env")
(define *buildfile-path* "/build")

(: write-recipe-to-dir ((struct recipe) string -> *))
(define (write-recipe-to-dir r dst)
  (let ((script (recipe-script r))
	(env    (real-env r)))
    (call-with-output-file
      (filepath-join dst *envfile-path*)
      (cut write-env env <>))
    (call-with-output-file
      (filepath-join dst *buildfile-path*)
      (cut write-exexpr script <>))
    (set-file-permissions! (filepath-join dst *buildfile-path*) #o744)))

(: with-tmpdir (forall (a) ((string -> a) -> a)))
(define (with-tmpdir proc)
  (let* ((dir (create-temporary-directory))
	 (res (proc dir)))
    (delete-directory dir #t)
    res))

(define *max-inline-file-size* 128)

(define (%file->string f)
  (call-with-input-file
    f
    (lambda (p)
      (let ((v (read-string #f p)))
	(if (eof-object? v) "" v)))))

;; %intern! interns an ordinary file
;; and returns the interned-file handle
(: %intern! (string string -> artifact))
(define (%intern! f abspath)
  (if (< (file-size f) *max-inline-file-size*)
      (let ((str (%file->string f)))
	(interned abspath (file-permissions f) str))
      (let* ((h   (hash-file f))
	     (dst (filepath-join (artifact-dir) h)))
	(begin
	  ;; TODO: this could be really slow for
	  ;; many/large outputs; there is some serious
	  ;; room for optimization here...
	  (unless (file-exists? dst)
	    (copy-file f dst #t 32768)
	    (set-file-permissions! dst #o600))
	  (%local-file abspath (file-permissions f) h)))))

(: dir->artifact (string -> artifact))
(define (dir->artifact dir)
  ;; NOTE: gzip embeds timestamps by default (sigh)
  (let* ((suffix ".tar.bz")
	 (format 'tar.bz)
	 (tmp    (create-temporary-file suffix)))
    ;; producing a fully-reproducible tar archive
    ;; is, unfortunately, a minefield
    (run
      "env"
      "LC_ALL=C.UTF-8" ;; necessary for --sort=name to be stable
      "tar"
      "-cjf" (abspath tmp)
      "--format=ustar"
      "--sort=name"
      ;; TODO: use owner-map and group-map for file ownership
      ;; when packages need files that aren't owned by uid=0
      "--owner=0"
      "--group=0"
      "--mtime=0"
      "--numeric-owner"
      "-C" dir ".")
    (let* ((h   (hash-file tmp))
	   (dst (filepath-join (artifact-dir) h)))
      (if (and (file-exists? dst) (equal? (hash-file dst) h))
	  (begin
	    (info "artifact already produced:" h)
	    (delete-file tmp))
	  (move-file tmp dst #t 32768))
      (local-archive format h))))

;; plan->outputs! builds a plan and yields
;; the list of interned file handles that are installed
;; into the /out directory in the jail
;;
;; all of the input plan's dependencies must
;; have been built (i.e. have plan-outputs)
;; in order for this to work
(: plan->outputs! ((struct plan) -> artifact))
(define (plan->outputs! p)
  (with-tmpdir
    (lambda (root)
      (define outdir (filepath-join root "out"))
      ;; unpack plan inputs and turn the recipe
      ;; into executables
      (trace "building in root" root)
      (create-directory (filepath-join root "sysroot") #t)
      (create-directory outdir #t)
      (for-each
	(lambda (p)
	  (let ((dir (filepath-join root (car p))))
	    (for-each
	      (lambda (in)
		(unpack!
		  (if (plan? in)
		      (or (plan-outputs in) (error "unbuilt plan" (plan-name in)))
		      in)
		  dir))
	      (cdr p))))
	(plan-inputs p))
      (write-recipe-to-dir (plan-recipe p) root)

      ;; fork+exec into the recipe inside the sandbox
      (sandbox-run
	root
	"/bin/emptyenv"
	"/bin/envfile" *envfile-path*
	*buildfile-path*)

      (dir->artifact outdir))))

(: build-plan! ((struct plan) -> *))
(define (build-plan! p)
  (define (do-plan! p)
    (info "building" (plan-name p))
    (save-plan-outputs! p (plan->outputs! p)))
  (plan-dfs
    (lambda (p)
      (when (plan? p)
	(let ((out (plan-outputs p)))
	  (or (and out (file-exists? (filepath-join (artifact-dir) (artifact-hash out))))
	      (do-plan! p)))))
    p))

(: build-package! (package-lambda conf-lambda conf-lambda -> artifact))
(define (build-package! proc host target)
  (let ((plan (%package->plan proc host target)))
    (build-plan! plan)
    (info (plan-name plan) (plan-hash plan) "is" (artifact-hash (plan-outputs plan)))
    (plan-outputs plan)))

;; write-digraph displays the dependency graph for a list of packages
;; in a format that can be used by the dot(1) tool
(: write-digraph (conf-lambda conf-lambda #!rest package-lambda -> *))
(define (write-digraph host target . pkgs)
  (let* ((plans (map (cut %package->plan <> host target) pkgs))
	 (ht    (make-hash-table)))
    (display "digraph packages {\n")
    (for-each
      (lambda (p)
	(plan-dfs
	  (lambda (p)
	    (when (and (plan? p)
		       (not (hash-table-ref/default ht p #f)))
	      (hash-table-set! ht p #t)
	      (for-each
		(lambda (in)
		  (write (plan-name p))
		  (display " -> ")
		  (cond
		    ((plan? in)
		     (write (plan-name in)))
		    ((artifact? in)
		     (case (vector-ref (artifact-format in) 0)
		       ;; try to produce a moderately informative textual representation...
		       ((archive)      (write (or (artifact-extra in) (artifact-hash in))))
		       ((file symlink) (write (vector-ref (artifact-format in) 1)))
		       (else           (write (artifact-hash in))))))
		  (display ";\n"))
		(apply append (map cdr (plan-inputs p))))))
	  p))
      plans)
    (display "}\n")))
