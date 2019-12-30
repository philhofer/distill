
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
  (let loop ((suff '((".tar.xz" . tar.xz)
		     (".tar.gz" . tar.gz)
		     (".tgz"    . tar.gz)
		     (".tar.bz" . tar.bz)
		     (".tar"    . tar))))
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
	   ((grp 0) : (or symbol integer)))

;; constructor for inlined interned file data
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
(define default-env
  '((PATH . "/bin:/sbin:/usr/bin:/usr/sbin:/usr/local/bin")
    (LANG . "en_US.UTF-8")
    (LC_ALL . "en_US.UTF-8")))

(defstruct recipe
	   ((env default-env) : (list-of pair))
	   (script : list))

(define-type conf-lambda (symbol -> *))
(define-type package-lambda (conf-lambda -> (struct package)))

;; package: a "portable" intermediate representation of a package
;; that is converted into a plan by combining it with the configuration
(defstruct package
	   (label : string)                    ;; human-readable name
	   (src : (struct leaf))               ;; where to get the package source
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
;; the saved-hash value if it has been cached
(define-type plan-input-type (or (struct plan) (struct leaf) (struct interned-file)))
(defstruct plan
	   (name : string)
	   (recipe : (struct recipe))
	   (inputs : (list-of plan-input-type))
	   ((saved-hash #f) : (or false string)))

(define (%plan name recipe inputs)
  (make-plan
    #:name name
    #:recipe recipe
    #:inputs inputs
    #:saved-hash (%plan-hash inputs recipe)))

#;(define-syntax intern-include
  (lambda (stx)
    (syntax-case stx ()
       ((_ fname abspath)
	(begin
	  (unless (string? fname)
	    (syntax-error "expected filename in intern-include form:" fname))
	  (unless (file-exists? fname)
	    (syntax-error "expected file to exist:" fname))
	  (let ((h (hash-file fname)))
	    #`(make-interned-file
		#:abspath abspath
		#:hash    ,h
		#:mode    #x0644)))))))

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
      (input-sort
	(cons
	  (package-src pkg)
	  (append (map (cut %package->plan <> host host) (package-tools pkg))
		  (map (cut %package->plan <> host target) (package-inputs pkg))
		  (package-overlay pkg)))))))

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
      (write (car p) prt)
      (write "=" prt)
      (write (cdr p) prt)
      (newline prt))
    env))

(: lookup (vector symbol --> *))
(define (lookup tbl sym)
  (let* ((len (vector-length tbl))
	 (h1  (symbol-hash sym len))
	 (ref (lambda (slot)
		(and-let* ((cell (vector-ref tbl slot))
			   (_    (eq? (car cell) sym)))
		  (cdr cell)))))
    (or (ref h1)
	(let ((h2 (symbol-hash sym len h1)))
	  (or (ref h2)
	      (let ((h3 (symbol-hash sym len h2)))
		(ref h3)))))))

;; table->proc turns a table into a single-argument
;; procecudure that returns associations
(: table->proc (vector -> (procedure (symbol) *)))
(define (table->proc tbl)
  (lambda (sym)
    (lookup tbl sym)))

;; table=? compares two tables using elem=?
;; to compare values associated with keys in each table
(: table=? (vector vector (procedure (* *) *) -> *))
(define (table=? left right elem=?)
  (define (super outer inner)
    (let ((len (vector-length outer)))
      (let loop ((i 0))
	(or (>= i len)
	    (let ((cell (vector-ref outer i)))
	      (or (eq? cell #f)
		  (let ((v (lookup inner (car cell))))
		    (and v (elem=? v (cdr cell)) (loop (+ i 1))))))))))
  (and (super left right)
       (super right left)))

;; (insert! table symbol value) creates
;; an association that can be later retreived with (lookup table symbol);
;; inserting the value #f is equivalent to removing that association
;; from the table
(: insert! (vector symbol * -> vector))
(define (insert! tbl sym val)

  ;; grow the table's capacity
  (define (grow! tbl)
    (let ((newvec (make-vector (ceiling (/ (* (vector-length tbl) 3) 2)) #f))
	  (oldlen (vector-length tbl)))
      (let loop ((i 0)
		 (vec newvec))
	(if (>= i oldlen)
	    vec
	    (let ((cell (vector-ref tbl i)))
	      (loop (+ i 1)
		    (if cell (insert! vec (car cell) (cdr cell)) vec)))))))

  ;; maybe insert a value, provided it is a pure overwrite
  (define (insert!? vec slot sym val)
    (let ((curval (vector-ref vec slot)))
      (and (or (not curval)
	       (eq? (car curval) sym))
	   (begin
	     (vector-set! vec slot (if val (cons sym val) val))
	     vec))))

  ;; decide when to grow the table
  (define (cycle? s depth)
    (or (and (>= depth 1)
	     (eq? s sym))
	(>= depth 10)))

  (let loop ((vec   tbl)
	     (sym   sym)
	     (val   val)
	     (depth 0))
    ;; try to insert in 3 probe positions
    (if (cycle? sym depth)
	(loop (grow! vec) sym val 0)
	(let* ((len (vector-length vec))
	       (h1  (symbol-hash sym len)))
	  (or (insert!? vec h1 sym val)
	      (let ((h2 (symbol-hash sym len h1)))
		(or (insert!? vec h2 sym val)
		    (let ((h3 (symbol-hash sym len h2)))
		      (or (insert!? vec h3 sym val)
			  ;; all probes failed, so either we bump the
			  ;; first probe position, or val is #f and we
			  ;; simply return (because a lookup would return #f)
			  (if val
			      (let ((oldval (vector-ref vec h1)))
				(vector-set! vec h1 (cons sym val))
				(loop vec (car oldval) (cdr oldval) (+ depth 1)))
			      vec))))))))))

(define table
  (case-lambda
    (() (make-vector 8 #f))
    ((alist) (let ((vec (make-vector (+ (* (length alist) 2) 1) #f)))
	       (foldl1
		 (lambda (v p)
		   (insert! v (car p) (cdr p)))
		 vec
		 alist)))))

;; tables are just vectors
(define table? vector?)

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
(: %plan-hash ((list-of plan-input-type) (struct recipe) --> string))
(define (%plan-hash inputs recipe)
  (define (write-inner x prt)
    (cond
      ((leaf? x)
       (write (leaf->alist x) prt))
      ((interned-file? x)
       (write (interned-file->alist x) prt))
      ((plan? x)
       (write (cons (plan-name x) (plan-hash x)) prt))))
  (hash-string
    (call-with-output-string
      (lambda (out)
	(for-each
	  (lambda (in)
	    (write-inner in out)
	    (newline out))
	  inputs)
	(write-env (recipe-env recipe) out)
	(write-exexpr (recipe-script recipe) out)))))

;; plan-hash returns the canonical hash of a plan
(: plan-hash ((struct plan) -> string))
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
			  (plan-inputs x)
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

(: sandbox-run (string string #!rest string -> undefined))
(define (sandbox-run root prog . args)
  (apply
    run
    "bwrap"
    "--unshare-ipc"
    "--unshare-pid"
    "--unshare-uts"
    "--unshare-cgroup"
    "--hostname" "builder"
    "--bind" root "/"
    "--dir" "/dev"
    "--dir" "/proc"
    "--dir" "/sysroot"
    "--dir" "/tmp"
    "--dev" "/dev"
    "--proc" "/proc"
    "--tmpfs" "/tmp"
    args))

;; ensure that a leaf artifact exists in the artifact dir,
;; or download it from the specified source and check its hash
(: wget-leaf (conf-lambda (struct leaf) -> *))
(define (wget-leaf conf l)
  (let* ((base (leaf-hash l))
	 (path (filepath-join (conf 'artifact-dir) base)))
    (or (and-let* ((h (hash-file path)))
	  (string=? h (leaf-hash l)))
	(let ((tmp (string-append path ".tmp")))
	  (info "fetching" base "from" (leaf-src l))
	  (run "wget" (leaf-src l) "-o" tmp)
	  (let ((res (hash-file tmp)))
	    (unless (and res (string=? res base))
	      (error "fetched artifact has the wrong hash:" res))
	    (rename-file path tmp #t))))))

;; unpack-leaf <conf> <leaf> <dst-dir>
;; unpacks a leaf artifact into dst-dir using tar(1)
(: unpack-leaf (conf-lambda (struct leaf) string -> *))
(define (unpack-leaf conf l dst)
  (let ((flags (case (leaf-format l)
		 ((tar.xz) "-Jxf")
		 ((tar.bz) "-jxf")
		 ((tar.gz) "-zxf")
		 ((tar)    "-xf")
		 (else     (error "unsupported leaf format" (leaf-format l)))))
	(lfile (filepath-join (conf 'artifact-dir) (leaf-hash l))))
    (unless (and-let* ((h (hash-file lfile)))
	      (string=? h (leaf-hash l)))
      (unless (leaf-src l)
	(error "leaf has no source and is not interned:" l))
      (let ((tmp (string-append lfile ".tmp")))
	(info "fetching" (leaf-hash l) "from" (leaf-src l))
	(run "wget" (leaf-src l) "-o" tmp)
	(let ((res (hash-file tmp)))
	  (unless (and res (string=? res (leaf-hash l)))
	    (delete-file tmp)
	    (error "fetch artifact has the wrong hash:" res))
	  (rename-file lfile tmp #t))))
    (trace "unpacking" (leaf-hash l) "to" dst)
    (run "tar" flags lfile "-C" dst)))

;; unpack an interned file 'i' to 'dst' using the artifacts
;; referenced through 'conf'
(: unpack-interned-file (conf-lambda (struct interned-file) string -> *))
(define (unpack-interned-file conf i dst)
  (let ((dstfile (filepath-join dst (interned-file-abspath i)))
	(perm    (interned-file-mode i))
	(srcfile (interned-file-cachepath conf i)))
    (copy-file srcfile dstfile #t 32768)
    (set-file-permissions! dstfile perm)))

;; plan-outputs-file is the file that stores the serialized
;; interned file information for a plan
(: plan-outputs-file (conf-lambda (struct plan) -> string))
(define (plan-outputs-file conf p)
  (filepath-join (plan-dir conf p) "outputs.scm"))

(: plan-dir (conf-lambda (struct plan) -> string))
(define (plan-dir conf p)
  (filepath-join (conf 'plan-dir) (plan-hash p)))

;; write 'lst' as the set of plan outputs associated with 'p'
(: save-plan-outputs! (conf-lambda (struct plan) (list-of (struct interned-file)) -> *))
(define (save-plan-outputs! conf p lst)
  (let* ((ifile->vec (lambda (ifile)
		       (vector
			 (interned-file-abspath ifile)
			 (interned-file-hash ifile)
			 (interned-file-mode ifile))))
	 (vecs (foldl1
		 (lambda (lst ifile)
		   (cons (ifile->vec ifile) lst))
		 '()
		 lst))
	 (vec  (list->vector vecs))
	 (srt  (sort vec (lambda (a b) (string<? (vector-ref a 0) (vector-ref b 0))))))
    ;; plan outputs are stored as a vector-of-vectors
    ;; like #(#("/bin/foo" hash #o644) ...) 
    ;; sorted by absolute filepath
    (create-directory (plan-dir conf p))
    (with-output-to-file
      (plan-outputs-file conf p)
      (lambda ()
	(write srt)))))

(: plan-outputs (conf-lambda (struct plan) -> (or list false)))
(define (plan-outputs conf p)
  (let ((desc (plan-outputs-file conf p)))
    (and (file-exists? desc)
	 (vector->list (with-input-from-file desc read)))))

;; plan-outputs-hash produces the canonical hash
;; for the set of outputs produced by a plan, or
;; #f if the set of outputs is unknown
(: plan-outputs-hash (conf-lambda (struct plan) -> (or false string)))
(define (plan-outputs-hash conf p)
  ;; TODO: need a sort here; can't just hash the text...
  (hash-file (plan-outputs-file conf p)))

;; plan outputs are stored as a list of serialized interned files
;; in <plan-dir>/<plan-hash>/outputs.scm
(: unpack-plan-outputs (conf-lambda (struct plan) string -> *))
(define (unpack-plan-outputs conf p dst)
  (for-each
    (lambda (vec)
      (unpack-interned-file
	conf
	;; XXX keep this in line with 'save-plan-outputs!'
	(make-interned-file
	  #:abspath (vector-ref vec 0)
	  #:hash    (vector-ref vec 1)
	  #:mode    (vector-ref vec 2))
	dst))
    (plan-outputs conf p)))

(: interned-file-cachepath (conf-lambda (struct interned-file) --> string))
(define (interned-file-cachepath conf f)
  (filepath-join (conf 'artifact-dir) (interned-file-hash f)))

(: fetch-sources! (conf-lambda (struct plan) -> undefined))
(define (fetch-sources! conf root)
  (plan-dfs
    (lambda (in)
      (cond
	((leaf? in)
	 (wget-leaf conf in))
	((interned-file? in)
	 (unless (file-exists? (interned-file-cachepath conf in))
	   (error "interned file doesn't exist:" in)))
	(else #t)))
    root))

;; unpack installs a plan input into the
;; given destination directory
(: unpack (conf-lambda plan-input-type string -> undefined))
(define (unpack conf i dst)
  (cond
    ((leaf? i) (unpack-leaf conf i dst))
    ((plan? i) (unpack-plan-outputs conf i dst))
    ((interned-file? i) (unpack-interned-file conf i dst))
    (else (error "unexpected value" i))))

(define *envfile-path* "/env")
(define *buildfile-path* "/build")

(: write-recipe-to-dir ((struct recipe) string -> *))
(define (write-recipe-to-dir r dst)
  (let ((script (recipe-script r))
	(env    (recipe-env r)))
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

;; %intern! moves a file to the artifact directory
;; and returns an interned-file handle
(: %intern! (conf-lambda string string -> (struct interned-file)))
(define (%intern! conf f abspath)
  (let* ((h    (hash-file f))
	 (perm (file-permissions f))
	 (dst  (filepath-join (conf 'artifact-dir) h)))
    ;; TODO: owner/group!
    (move-file f dst #t 32768)
    (set-file-permissions! dst #o600)
    (make-interned-file
      #:abspath abspath
      #:mode    perm
      #:hash    h)))

;; plan->outputs! builds a plan and yields
;; the list of interned file handles that are installed
;; into the /sysroot directory in the jail
(: plan->outputs! (conf-lambda (struct plan) -> (list-of (struct interned-file))))
(define (plan->outputs! conf p)
  (with-tmpdir
    (lambda (root)
      (trace "building in root" root)
      (for-each
	(cut unpack conf <> root)
	(plan-inputs p))
      (write-recipe-to-dir (plan-recipe p) root)
      (sandbox-run
	root
	"/bin/emptyenv"
	"/bin/envfile" *envfile-path*
	*buildfile-path*)
      (let* ((outdir (filepath-join root "sysroot"))
	     (basep  (lambda (f)
		       (if (string-prefix? outdir f)
			   (filepath-join "/" (substring/shared f (string-length outdir)))
			   (error "file not in dir" f outdir)))))
	(find-files
	  outdir
	  #:action (lambda (f lst)
		     (cons (%intern! conf f (basep f)) lst)))))))

;; enter a build with the given configuration
;; and root plan
(: build-tree! (conf-lambda (struct plan) -> *))
(define (build-tree! conf p)
  (define (do-plan! conf p)
    (save-plan-outputs! conf p (plan->outputs! conf p)))
  (plan-dfs
    (lambda (p)
      ;; TODO: testing that the output file exists doesn't guarantee
      ;; that unpacking this plan's outputs will succeed; however,
      ;; checking the plan outputs individually each time is probably
      ;; overkill...
      (when (and (plan? p) (not (file-exists? (plan-outputs-file conf p))))
	(do-plan! conf p)))
    p))

(: build-package! (package-lambda conf-lambda conf-lambda -> (struct plan)))
(define (build-package! proc host target)
  (let ((plan (%package->plan proc host target)))
    (build-tree! target plan)
    plan))
