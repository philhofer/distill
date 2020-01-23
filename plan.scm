

;; artifacts are just vectors
;;
;; note that artifact-extra must not contain
;; data that changes the filesystem representation
;; of the artifact (it is ignored for plan hashing purposes)
(define-type artifact (vector vector string *))
(: %artifact (forall (a) (vector string a --> (vector vector string a))))
(define (%artifact format hash extra)
  (vector format hash extra))

(: artifact-format (artifact -> vector))
(define (artifact-format v) (vector-ref v 0))
(: artifact-hash (artifact -> string))
(define (artifact-hash v)   (vector-ref v 1))
(: artifact-extra (artifact -> *))
(define (artifact-extra v)  (vector-ref v 2))
(define (artifact? v) (and (vector? v) (= (vector-length v) 3)))

;; short-hash is a 6-character hash prefix
(define (short-hash h)
  (substring/shared h 0 6))

;; artifact-repr converts an artifact to its
;; external representation (which omits metadata
;; that doesn't change the contents of the artifact)
(: artifact-repr (forall (a) (string (vector a string *) --> (vector string a string))))
(define (artifact-repr root v)
  (vector root (artifact-format v) (artifact-hash v)))

;; guess the format of a remote source bundle
(define (impute-format src)
  (let loop ((suff '((".tar.xz" . tar.xz)
                     (".tar.gz" . tar.gz)
                     (".tgz"    . tar.gz)
                     (".tar.bz" . tar.bz)
                     (".tar.bz2". tar.bz)
                     (".tar.zst". tar.zst)
                     (".tar"    . tar))))
    (cond
      ((null? suff) (error "bad archive suffix" src))
      ((string-suffix? (caar suff) src) (cdar suff))
      (else (loop (cdr suff))))))

;; TODO: normalize remote archives for faster re-builds
;; (large xz tarballs are reeeeally slow to unpack)
(: remote-archive (string string #!rest * --> artifact))
(define (remote-archive src hash #!key (kind #f))
  (%artifact
    `#(archive ,(or kind (impute-format src)))
    hash
    src))

(: fetch-remote-file! (string string string integer -> artifact))
(define (fetch-remote-file! src hash abspath mode)
  (unless (file-exists? (filepath-join (artifact-dir) hash))
    (fetch! src hash))
  (%local-file abspath mode hash))

;; import-archive! imports an archive from
;; a given location in the filesystem by
;; symlinking it into the artifacts directory
(: import-archive! (string -> artifact))
(define (import-archive! path)
  (let ((h    (hash-file path))
        (kind (impute-format path)))
    (unless h (fatal "file doesn't exist:" path))
    (let ((artpath (filepath-join (artifact-dir) h)))
      (or (file-exists? artpath)
          (copy-file path (abspath artpath) #f 32768))
      (%artifact
        `#(archive ,kind)
        h
        #f))))

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
(define-type plan-input-type (or (struct plan) artifact))
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

(define (plan-resolved? p)
  (let loop ((lst (plan-inputs p)))
    (or (null? lst)
        (let ((head (car lst))
              (rest (cdr lst)))
          (and (let inner ((lst (cdr head)))
                 (or (null? lst)
                     (and (or (artifact? (car lst))
                              (plan-outputs (car lst)))
                          (inner (cdr lst)))))
               (loop rest))))))

(: %write-plan-inputs
   ((list-of
      (pair string (list-of plan-input-type)))
    (struct recipe)
    output-port -> undefined))
(define (%write-plan-inputs inputs recipe out)
  (define (->artifact in)
    (if (plan? in)
      (or (plan-outputs in)
          (error "plan not resolved"))
      in))
  ;; sort input artifacts by extraction directory
  ;; then by content hash
  (: art<? (artifact artifact --> boolean))
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
  ;;   #("/" #('archive 'tar.xz)            "...<hash>...") ...)
  (define (cons*-inputs in tail)
    (foldl1
      (lambda (lst p)
        (cons*-reprs (car p) (cdr p) lst))
      tail
      in))

  ;; write plans in a format that can be deserialized;
  ;; that way we can build old plans even if we don't
  ;; have the code that generated them (as long as
  ;; we have the original artifacts)
  (let ((inputs (sort (cons*-inputs inputs '()) art<?)))
    (write
      (list (cons 'inputs inputs)
            (cons 'env (real-env recipe))
            (cons 'script (with-output-to-string
                            (lambda () (write-exexpr (recipe-script recipe))))))
      out)))

;; plan-hash returns the canonical hash of a plan,
;; or #f if any of its inputs have unknown outputs
(: plan-hash ((struct plan) -> (or false string)))
(define (plan-hash p)
  (or (plan-saved-hash p)
      (and (plan-resolved? p)
           (let* ((str (call-with-output-string
                         (cute
                           %write-plan-inputs (plan-inputs p) (plan-recipe p) <>)))
                  (h   (hash-string str))
                  (dir (filepath-join (plan-dir) h))
                  (ofd (string-append dir "/inputs.scm"))
                  (lfd (string-append dir "/label")))
             (unless (file-exists? ofd)
               (create-directory dir #t)
               (call-with-output-file ofd (cut write-string str <>))
               (call-with-output-file lfd (cute display (plan-name p) <>)))
             (plan-saved-hash-set! p h)
             h))))

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
    (info "  -- fetching" src)
    (run "wget" "-q" "-O" tmp src)
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
      (error "can't save outputs for plan (unresolved inputs):" (plan-name p)))
    (let ((old (plan-outputs p)))
      (cond
        ((not old)
         (begin
           (create-directory (dirname outfile))
           (with-output-to-file outfile (lambda () (write ar)))
           (plan-saved-output-set! p ar)))
        ((equal? old ar)
         (info "plan for" (plan-name p) "reproduced" (short-hash (plan-hash p))))
        (else
          (fatal "plan for" (plan-name p) "failed to reproduce:" old ar))))))

;; determine the outputs (leaf) of the given plan,
;; or #f if the plan has never been built with its inputs
(: plan-outputs ((struct plan) -> (or artifact false)))
(define (plan-outputs p)
  (or (plan-saved-output p)
      (let ((desc (plan-outputs-file p)))
        (and desc (file-exists? desc)
             (let ((vec (with-input-from-file desc read)))
               (unless (artifact? vec)
                 (error "unexpected plan format" vec))
               (plan-saved-output-set! p vec)
               vec)))))

(define cdn-url (make-parameter
                  "https://b2cdn.sunfi.sh/file/pub-cdn/"))

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
    (let ((comp (case kind
                  ((tar.gz) "-z")
                  ((tar.bz) "-j")
                  ((tar.xz) "-J")
                  ((tar.zst) "--zstd")
                  ((tar)    "")
                  (else (error "unknown/unsupported archive kind" kind))))
          (infile  (filepath-join (artifact-dir) hash)))
      (when (not (file-exists? infile))
        (fetch! (or src
                    (begin
                      (info "  -- trying to fetch" (short-hash hash) "from fallback cdn...")
                      (string-append (cdn-url) hash)))
                hash))
      (create-directory dst #t)
      (run "tar" comp "-xkf" infile "-C" dst)))

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
    (with-output-to-file
      (filepath-join dst *buildfile-path*)
      (lambda () (write-exexpr script)))
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
  (let* ((suffix ".tar.zst")
         (format 'tar.zst)
         (tmp    (create-temporary-file suffix)))
    ;; producing a fully-reproducible tar archive
    ;; is, unfortunately, a minefield
    (run
      "env"
      "LC_ALL=C.UTF-8" ;; necessary for --sort=name to be stable
      "tar"
      ;; TODO: make sure zstd is invoked with explicit
      ;; compression-level and threading arguments to keep
      ;; that from being a possible source of reproducibility issues,
      ;; _or_ calculate the checksum on the uncompressed archive
      "--zstd"
      "-cf" (abspath tmp)
      "--format=ustar"
      "--sort=name"
      ;; TODO: use owner-map and group-map for file ownership
      ;; when packages need files that aren't owned by uid=0
      "--owner=0"
      "--group=0"
      "--mtime=@0"
      "--numeric-owner"
      "-C" dir ".")
    (let* ((h   (hash-file tmp))
           (dst (filepath-join (artifact-dir) h)))
      (if (and (file-exists? dst) (equal? (hash-file dst) h))
        (begin
          (info "  -- artifact reproduced:" (short-hash h))
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
      (create-directory outdir #t)
      (for-each
        (lambda (p)
          (let ((dir (filepath-join root (car p))))
            (for-each
              (lambda (in)
                (unpack!
                  (if (plan? in)
                    (or
                      (begin
                        (info "  -- unpacking output of" (plan-name in))
                        (plan-outputs in))
                      (fatal "unbuilt plan prerequisite:" (plan-name in)))
                    in)
                  dir))
              (cdr p))))
        (plan-inputs p))
      (write-recipe-to-dir (plan-recipe p) root)

      ;; fork+exec into the recipe inside the sandbox
      (info "  -- running build script ...")
      (sandbox-run
        root
        "/bin/emptyenv"
        "/bin/envfile" *envfile-path*
        ;; close stdin and redirect stdin+stderr to a log file
        ;; (otherwise builds just get really noisy in the terminal)
        "redirfd" "-r" "0" "/dev/null"
        "redirfd" "-w" "1" "/build.log"
        "fdmove" "-c" "2" "1"
        *buildfile-path*)

      ;; save the compressed build log independently
      ;; (it's not 'reproduced' as such) and delete
      ;; the old log
      (let* ((pdir  (filepath-join (plan-dir) (plan-hash p)))
             (ofile (filepath-join pdir "build.log.zst")))
        ;; the zstd tool will complain if we reproduce an ouput,
        ;; so only keep one build log at a time
        (when (file-exists? ofile)
          (delete-file* ofile))
        (create-directory pdir #t)
        (run "zstd"
             "-q"
             "--rm" (filepath-join root "build.log")
             "-o"   ofile))
      (dir->artifact outdir))))

(: plan-built? ((struct plan) -> boolean))
(define (plan-built? p)
  (and-let* ((out (plan-outputs p)))
    (file-exists?
      ;; TODO: if we're here, we already know the plan outputs,
      ;; so we could try to fetch the output from the CDN
      ;; instead of performing a re-build
      (filepath-join (artifact-dir) (artifact-hash out)))))

(: build-plan! ((struct plan) #!rest * -> *))
(define (build-plan! top #!key (rebuild #f))
  (define (do-plan! p)
    (fatal "refusing rebuild:" (plan-name p))
    (info "building" (plan-name p))
    (save-plan-outputs! p (plan->outputs! p)))
  (plan-dfs
    (lambda (p)
      (when (and
              (plan? p)
              (or (not (plan-built? p)) (and (eq? p top) rebuild)))
        (do-plan! p)))
    top))

