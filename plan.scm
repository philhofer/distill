
;; leaf: a node in the rootfs DAG with no input vertices
;;
;; since leaves have no recipe (beyond downloading something),
;; they don't need reproducible recipes, and can thus accomodate
;; non-reproducible formats like tar, etc.
(defstruct leaf
	   (name : string)
	   (src : (or string false))
	   (hash : string)
	   ((format 'verbatim) : symbol))

;; guess the format of a remote source bundle
(define (impute-format src)
  (unless (string-prefix? "https://" src)
    (error "source spec doesn't begin with https://:" src))
  (let loop ((suff '((".tar.xz" tar xz)
		     (".tar.gz" tar gz)
		     (".tgz"    tar gz)
		     (".tar.bz" tar bz)
		     (".tar"    tar))))
    (cond
      ((null? suff) 'unknown)
      ((string-suffix? (caar suff) src) (cdar suff))
      (else (loop (cdr suff))))))

;; plan outputs are an unordered set of interned files
;; (files are marked by their *desired* mode+owner in
;; the output root filesystem, not the mode+owner when
;; they are gathered from the input directory)
(defstruct interned-file
	   (abspath : string)
	   (hash : string)
	   ((mode #o644) : integer)
	   ((contents #f) : (or false string u8vector)) ;; for inline interned-files
	   ((usr 0) : (or symbol integer))
	   ((grp 0) : (or symbol integer))
	   ((symlink? #f) : boolean)) ;; is symlink

;; by default, dump stuff into these directories
;; TODO: inherit these from the environment
(define plan-dir (make-parameter "./plans"))
(define artifact-dir (make-parameter "./artifacts"))

(define (%ifile->vector i)
  (vector
    (interned-file-abspath i)
    (interned-file-hash i)
    (interned-file-mode i)
    (interned-file-contents i)
    (interned-file-usr i)
    (interned-file-grp i)
    (interned-file-symlink? i)))

(define (%vector->ifile v)
  (make-interned-file
    #:abspath  (vector-ref v 0)
    #:hash     (vector-ref v 1)
    #:mode     (vector-ref v 2)
    #:contents (vector-ref v 3)
    #:usr      (vector-ref v 4)
    #:grp      (vector-ref v 5)
    #:symlink? (vector-ref v 6)))

;; constructor for inlined interned file data
(: interned (string integer string -> (struct interned-file)))
(define (interned abspath mode data)
  (make-interned-file
    #:abspath abspath
    #:hash (cond
	     ((string? data) (hash-string data))
	     ((bytevector? data) (hash-bytevector data))
	     (else (error "unexpected data argument to 'interned'" data)))
    #:mode mode
    #:contents data))

;; default environment for recipes
(define *default-env*
  '((PATH . "/bin:/sbin:/usr/bin:/usr/sbin:/usr/local/bin")
    (LC_ALL . "C.UTF-8")))

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
	   (src : (or (struct leaf) (list-of (struct leaf)))) ;; where to get the package source
	   (tools : (list-of package-lambda))  ;; build tools (built for host)
	   (inputs : (list-of package-lambda)) ;; build dependencies (built for target)
	   (build : (struct recipe))           ;; build script (see execline*)
	   ((overlay '()) : (list-of (struct interned-file)))) ;; additional files to include (patches, etc.)

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
(define-type plan-input-type (or (struct plan) (struct leaf) (struct interned-file)))
(defstruct plan
	   (name : string)
	   ;; recipe is the code+environment to be executed
	   (recipe : (struct recipe))
	   ;; inputs are the set of filesystem inputs
	   ;; represented as an alist of root directories
	   ;; and contents to be extracted relative to those roots
	   ;; (see 'unpack')
	   (inputs : (list-of
		       (pair string (or plan-input-type (list-of plan-input-type)))))
	   ((saved-hash #f) : (or false string))
	   ((saved-output #f) : (or false (struct leaf))))

(defstruct artifact
           (abspath : string)
	   (method  : (or symbol (list-of symbol))) ;; verbatim, symlink, or (tar ..)
	   (hash    : string)
	   ((data #f) : (or false string u8vector)) ;; inline contents
	   ((remote #f) : (or false string))) ;; remote source

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
(define (%package->plan pkg-proc host target)
  ;; we need some sort of order for plan inputs
  ;; in order for plan hashes to be consistent;
  ;; this is hokey but it'll do for now...
  (define (input-sort plans)
    (define (ord x)
      (cond
        ((plan? x) (values 0 (plan-name x)))
        ((leaf? x) (values 1 (leaf-name x)))
        ((interned-file? x) (values 2 (interned-file-abspath x)))
	(else (error "unexpected value" x))))
    (sort
      plans
      (lambda (a b)
	(let-values (((anum astr) (ord a))
		     ((bnum bstr) (ord b)))
	  (or (< anum bnum)
	      (and (= anum bnum)
		   (string< astr bstr)))))))
  (let ((pkg (pkg-proc target)))
    (%plan
      (require ok-plan-name? (package-label pkg))
      (package-build pkg)
      ;; NOTE: plan-hash is sensitive to the sort here
      (list
	(cons "/" (input-sort
		    (flatten (package-src pkg)
			     (package-overlay pkg)
			     (map (cut %package->plan <> host host)
				  (package-tools pkg)))))
	(cons "/sysroot" (input-sort
			   (map (cut %package->plan <> host target)
				(package-inputs pkg))))))))

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
      (define (leaf-repr l root)
	(list 'leaf root (leaf-hash l) (leaf-format l)))
      (define (file-repr f root)
	(if (interned-file-symlink? f)
	    (list 'symlink root (interned-file-abspath f) (interned-file-contents f))
	    (list 'file root (interned-file-abspath f) (interned-file-hash f) (interned-file-mode f))))
      (define (write-input in root prt)
	(cond
	  ((leaf? in)
	   (write (leaf-repr in root) prt))
	  ((plan? in)
	   (let ((out (plan-outputs in)))
	     (if out
	         (write (leaf-repr out root) prt)
		 (ret #f))))
	  ((interned-file? in)
	   (write (file-repr in root) prt)))
	(newline prt))
      (define (write-assoc p prt)
	(for-each
	  (cute write-input <> (car p) prt)
	  (cdr p)))
      (hash-string
	(call-with-output-string
	  (lambda (out)
	    (for-each (cut write-assoc <> out) inputs)
	    (write-env (real-env recipe) out)
	    (write-exexpr (recipe-script recipe) out)))))))

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

;; memoize a procedure by remembering
;; its arguments (using eq? for equality)
(: memoize-eq (forall (a b) ((a -> b) -> (a -> b))))
(define (memoize-eq proc)
  (let ((results '()))
    (lambda (in)
      (or (and-let* ((p (assq in results)))
	    (cdr p))
	  (let ((res (proc in)))
	    (set! results (cons (cons in res) results))
	    res)))))

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

;; unpack-leaf <conf> <leaf> <dst-dir>
;; unpacks a leaf artifact into dst-dir using tar(1)
(: unpack-leaf ((struct leaf) string -> *))
(define (unpack-leaf l dst)
  (let ((flags (case (leaf-format l)
		 ((tar.xz) "-Jkxf")
		 ((tar.bz) "-jkxf")
		 ((tar.gz) "-zkxf")
		 ((tar)    "-kxf")
		 (else     (error "unsupported leaf format" (leaf-format l)))))
	(lfile (filepath-join (artifact-dir) (leaf-hash l))))
    (unless (and-let* ((h (hash-file lfile)))
	      (or (string=? h (leaf-hash l))
		  (fatal "file exists but has bad hash:" lfile h (leaf-hash l))))
      (unless (leaf-src l)
	(fatal "leaf has no source and is not interned:" (leaf-name l) lfile))
      (let ((tmp (string-append lfile ".tmp")))
	(info "fetching" (leaf-hash l) "from" (leaf-src l))
	(run "wget" (leaf-src l) "-O" tmp)
	(let ((res (hash-file tmp)))
	  (unless res
	    (fatal "wget didn't produce the file:" tmp))
	  (unless (string=? res (leaf-hash l))
	    #;(delete-file tmp)
	    (fatal "fetched artifact has the wrong hash:" res "not" (leaf-hash l)))
	  (rename-file tmp lfile #t))))
    (trace "unpacking" (leaf-hash l) "to" dst)
    (run "tar" flags lfile "-C" dst)))

;; unpack an interned file 'i' to 'dst'
(: unpack-interned-file ((struct interned-file) string -> *))
(define (unpack-interned-file i dst)
  (let ((dstfile (filepath-join dst (interned-file-abspath i)))
	(perm    (interned-file-mode i)))
    (trace "unpack" (interned-file-abspath i) "->" dstfile)
    ;; TODO: handle usr and grp;
    ;; we're not typically root when unpacking, so
    ;; it's not clear how we can chown/chgrp under
    ;; ordinary circumstances
    (create-directory (dirname dstfile) #t)
    (cond
      ((interned-file-symlink? i)
       (create-symbolic-link (interned-file-contents i) dstfile))
      ((interned-file-contents i)
       (begin
	 (with-output-to-file dstfile (lambda () (write-string (interned-file-contents i))))
	 (set-file-permissions! dstfile perm)))
      (else
	(let ((srcfile (interned-file-cachepath i)))
	  (unless (file-exists? srcfile)
	    (error "cannot unpack file (doesn't exist):" dstfile))
	  (copy-file srcfile dstfile #t 32768)
	  (set-file-permissions! dstfile perm))))))

;; plan-outputs-file is the file that stores the serialized
;; interned file information for a plan
(: plan-outputs-file ((struct plan) -> (or string false)))
(define (plan-outputs-file p)
  (and-let* ((h (plan-hash p)))
    (filepath-join (plan-dir) h "outputs.scm")))

;; write 'lst' as the set of plan outputs associated with 'p'
(: save-plan-outputs! ((struct plan) (struct leaf) -> *))
(define (save-plan-outputs! p lf)
  (let ((outfile (plan-outputs-file p)))
    (unless outfile
      (error "can't save outputs for plan:" (plan-name p)))
    (create-directory (dirname outfile))
    (with-output-to-file
      outfile
      (lambda () (write (vector (leaf-format lf) (leaf-hash lf)))))
    (plan-saved-output-set! p lf)))

;; determine the outputs (leaf) of the given plan,
;; or #f if the plan has never been built with its inputs
(: plan-outputs ((struct plan) -> (or (struct leaf) false)))
(define (plan-outputs p)
  (let ((saved (plan-saved-output p))
	(desc  (plan-outputs-file p)))
    (or saved
	(and desc (file-exists? desc)
	     (let ((vec (with-input-from-file desc read)))
	       (and (vector? vec)
		    (= (vector-length vec) 2)
		    (let ((leaf (make-leaf
				  #:src #f
				  #:format (vector-ref vec 0)
				  #:hash   (vector-ref vec 1))))
		      (plan-saved-output-set! p leaf)
		      leaf)))))))

;; unpack a plan (turn it into a leaf and then unpack the leaf)
(: unpack-plan-outputs ((struct plan) string -> *))
(define (unpack-plan-outputs p dst)
  (let ((out (plan-outputs p)))
    (unless out
      (error "unpack-plan-outputs on unbuilt plan" (plan-name p)))
    (unpack-leaf out dst)))

(: interned-file-cachepath ((struct interned-file) --> string))
(define (interned-file-cachepath f)
  (filepath-join (artifact-dir) (interned-file-hash f)))

;; unpack installs a plan input into the
;; given destination directory
(: unpack (plan-input-type string -> undefined))
(define (unpack i dst)
  (cond
    ((leaf? i) (unpack-leaf i dst))
    ((plan? i) (unpack-plan-outputs i dst))
    ((interned-file? i) (unpack-interned-file i dst))
    (else (error "unpack: unexpected value" i))))

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
(: %intern! (string string -> (struct interned-file)))
(define (%intern! f abspath)
  (trace "intern file" f "->" abspath)
  (if (< (file-size f) *max-inline-file-size*)
      (let ((str (%file->string f)))
	(make-interned-file
	  #:abspath abspath
	  #:mode    (file-permissions f)
	  #:contents str
	  #:hash     (hash-string str)))
      (let* ((h   (hash-file f))
	     (dst (filepath-join (artifact-dir) h)))
	(begin
	  ;; TODO: this could be really slow for
	  ;; many/large outputs; there is some serious
	  ;; room for optimization here...
	  (unless (file-exists? dst)
	    (copy-file f dst #t 32768)
	    (set-file-permissions! dst #o600))
	  (make-interned-file
	    #:abspath abspath
	    #:mode    (file-permissions f)
	    #:hash    h)))))

;; %intern-link returns a link
;; at abspath linking to lnk
(: %intern-link (string string -> (struct interned-file)))
(define (%intern-link abspath lnk)
  (trace "intern symlink" abspath "->" lnk)
  (make-interned-file
    #:abspath  abspath
    #:contents lnk
    #:symlink? #t))

(: dir->leaf (string -> (struct leaf)))
(define (dir->leaf dir)
  (let* ((suffix ".tar.gz")
	 (format 'tar.gz)
	 (tmp    (filepath-join (dirname dir) (string-append "out" suffix))))
    ;; producing a fully-reproducible tar archive
    ;; is, unfortunately, a minefield
    (run
      "env"
      "LC_ALL=C.UTF-8" ;; necessary for --sort=name to be stable
      "tar"
      "-czf" (abspath tmp)
      "--format=pax"
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
      (if (file-exists? dst)
	  (begin
	    (info "artifact already produced:" h)
	    (delete-file tmp))
	  (move-file tmp dst #f 32768))
      (make-leaf
	#:src   #f
	#:hash   h
	#:format format))))

;; plan->outputs! builds a plan and yields
;; the list of interned file handles that are installed
;; into the /out directory in the jail
(: plan->outputs! ((struct plan) -> (struct leaf)))
(define (plan->outputs! p)
  (with-tmpdir
    (lambda (root)
      (define outdir (filepath-join root "out"))
      ;; unpack plan inputs and turn the recipe
      ;; into executables
      (trace "building in root" root)
      (create-directory (filepath-join root "sysroot") #t)
      (create-directory outdir #t)
      (let ((in (plan-inputs p)))
	(for-each
	  (lambda (p)
	    (let ((dir (filepath-join root (car p))))
	      (for-each
		(cut unpack <> dir)
		(cdr p))))
	  in))
      (write-recipe-to-dir (plan-recipe p) root)

      ;; fork+exec into the recipe inside the sandbox
      (sandbox-run
	root
	"/bin/emptyenv"
	"/bin/envfile" *envfile-path*
	*buildfile-path*)

      (dir->leaf outdir))))

(: build-plan! ((struct plan) -> *))
(define (build-plan! p)
  (define (do-plan! p)
    (info "building" (plan-name p))
    (save-plan-outputs! p (plan->outputs! p)))
  (plan-dfs
    (lambda (p)
      (when (plan? p)
	(let ((out (plan-outputs p)))
	  (or out
	      (do-plan! p))
	  (info (plan-name p) "is" (leaf-hash (plan-outputs p))))))
    p))

(: build-package! (package-lambda conf-lambda conf-lambda -> (struct leaf)))
(define (build-package! proc host target)
  (let ((plan (%package->plan proc host target)))
    (build-plan! plan)
    (plan-outputs plan)))
