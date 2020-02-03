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

(define info-prefix (make-parameter ""))

(define (infoln . args) (apply info (info-prefix) args))

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

(: remote-file ((or false string) string string fixnum --> artifact))
(define (remote-file src hash abspath mode)
  (%artifact
    `#(file ,abspath ,mode)
    hash
    (cons 'remote src)))

(: local-archive (symbol string --> artifact))
(define (local-archive kind hash)
  (%artifact
    `#(archive ,kind)
    hash
    #f))

(: interned (string integer string --> artifact))
(define (interned abspath mode contents)
  (%artifact
    `#(file ,abspath ,mode)
    (hash-string contents)
    (cons 'inline contents)))

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
;; the entry point for a plan build is the executable "/build"
;; with the environment from "/env," keeping in mind that
;; the environment itself is set with /bin/envfile, so you'll
;; need at least execline-tools in /bin no matter what
;;
;; DO NOT USE SETTERS on this structure; you will invalidate
;; the saved-* values that are used to reduce the amount
;; of dependency graph traversal necessary to produce plan input and output hashes
(define-type plan-input-type (or (struct plan) artifact))
(defstruct plan
  (name : string)
  ;; inputs are the set of filesystem inputs
  ;; represented as an alist of root directories
  ;; and contents to be extracted relative to those roots
  ;; (see 'unpack')
  (inputs : (list-of
              (pair string (list-of plan-input-type))))
  ((saved-hash #f) : (or false string))
  ((saved-output #f) : (or false vector))
  ((parallel #t) : (or boolean symbol)))

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

;; input-seq generates a sequence from plan inputs
(define (input-seq pl)
  (lambda (kons seed)
    (let outer ((inputs (plan-inputs pl))
                (out    seed))
      (if (null? inputs)
        out
        (let inner ((lst  (cdar inputs))
                    (out  out))
          (if (null? lst)
            (outer (cdr inputs) out)
            (inner (cdr lst) (kons (car lst) out))))))))

;; not defined in R7RS, but trivial enough to implement
(: call-with-output-string ((output-port -> *) -> string))
(define (call-with-output-string proc)
  (let ((p (open-output-string)))
    (proc p)
    (let ((s (get-output-string p)))
      (close-output-port p)
      s)))

;; plan-resolved? returns whether or not
;; all of the inputs to this plan are resolved
;; (i.e. artifacts or plans with known outputs)
(define (plan-resolved? p)
  (all/s? (lambda (in)
            (or (artifact? in)
                (plan-outputs in)))
          (input-seq p)))

(: %write-plan-inputs
   ((list-of
      (pair string (list-of plan-input-type)))
    output-port -> undefined))
(define (%write-plan-inputs inputs out)
  ;; turn an input into an artifact unconditionally
  (define (->artifact in)
    (if (plan? in)
      (or (plan-outputs in)
          (error "plan not resolved"))
      in))

  ;; sort input artifacts by extraction directory,
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

  ;; pair->inseq produces a sequence from an input pair like
  ;;  ("/" . (artifacts ...))
  ;; where the sequence elements are the external representation
  ;; of the artifacts in the cdr of the pair
  (define (pair->inseq p)
    (let ((rt  (car p))
          (lst (cdr p)))
      (s/map
        (lambda (in)
          (artifact-repr rt (->artifact in)))
        (list->seq lst))))

  ;; write plans in a format that can be deserialized;
  ;; that way we can build old plans even if we don't
  ;; have the code that generated them (as long as
  ;; we have the original artifacts)
  (let* ((inputs ((list->seq inputs) (k/map pair->inseq (k/recur cons)) '()))
         (inputs (sort inputs art<?)))
    (write inputs out)))

;; plan-hash returns the canonical hash of a plan,
;; or #f if any of its inputs have unknown outputs
(: plan-hash ((struct plan) -> (or false string)))
(define (plan-hash p)
  (or (plan-saved-hash p)
      (and (plan-resolved? p)
           (let* ((str (call-with-output-string
                         (cute
                           %write-plan-inputs (plan-inputs p) <>)))
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
                        ((input-seq x) cons '())
                        '()))))

;; fork+exec, wait for the process to exit and check
;; that it exited successfully
(: run (string #!rest string -> undefined))
(define (run prog . args)
  (trace "run" (cons prog args))
  (let-values (((pid ok? status) (process-wait/yield (process-run prog args))))
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

(: fetch! ((or false string) string -> *))
(define (fetch! src hash)
  (let* ((url (or src (begin
                        (infoln "trying to fetch" (short-hash hash) "from fallback CDN")
                        (string-append (cdn-url) hash))))
         (dst (filepath-join (artifact-dir) hash))
         (tmp (string-append dst ".tmp")))
    (infoln "fetching" src)
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
         (infoln "plan for" (plan-name p) "reproduced" (short-hash (plan-hash p))))
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

;; unpack! unpacks an artifact at a given root directory
(: unpack! (artifact string -> *))
(define (unpack! i dst)

  (define (unpack-file dst abspath mode hash content)
    (let ((dstfile (filepath-join dst abspath))
          (artfile (filepath-join (artifact-dir) hash)))
      ;; ensure that the file is present in artifacts/
      ;; before copying it into the destination
      (when (not (file-exists? artfile))
        (match content
          (`(inline . ,content)
           (with-output-to-file artfile (lambda () (write-string content))))
          (`(remote . ,url)
           (fetch! url hash))
          (#f
           (fetch! #f hash))
          (else
            (error "unrecognized file content spec:" content))))
      (create-directory (dirname dstfile) #t)
      (copy-file (filepath-join (artifact-dir) hash) dstfile)
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
        (fetch! src hash))
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

(: with-tmpdir (forall (a) ((string -> a) -> a)))
(define (with-tmpdir proc)
  (let* ((dir (create-temporary-directory))
         (_   (set-file-permissions! dir #x700))
         (res (proc dir)))
    (delete-directory dir #t)
    res))

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
      "--owner=0"
      "--group=0"
      "--mtime=@0"
      "--numeric-owner"
      "-C" dir ".")
    (let* ((h   (hash-file tmp))
           (dst (filepath-join (artifact-dir) h)))
      (if (and (file-exists? dst) (equal? (hash-file dst) h))
        (begin
          (infoln "artifact reproduced:" (short-hash h))
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
(: plan->outputs! ((struct plan) fixnum -> artifact))
(define (plan->outputs! p nproc)
  (with-tmpdir
    (lambda (root)
      (define outdir (filepath-join root "out"))
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
                        (infoln "unpacking output of" (plan-name in))
                        (plan-outputs in))
                      (fatal "unbuilt plan prerequisite:" (plan-name in)))
                    in)
                  dir))
              (cdr p))))
        (plan-inputs p))

      ;; fork+exec into the recipe inside the sandbox
      (infoln "running build script ...")
      (sandbox-run
        root
        "/bin/emptyenv"
        "/bin/envfile" "/env"
        ;; close stdin and redirect stdin+stderr to a log file
        ;; (otherwise builds just get really noisy in the terminal)
        "redirfd" "-r" "0" "/dev/null"
        "redirfd" "-w" "1" "/build.log"
        "fdmove" "-c" "2" "1"
        "export" "nproc" (number->string nproc)
        "/build")

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

(define (input-map proc p)
  (let loop ((lst (plan-inputs p))
             (out '()))
    (if (null? lst)
      out
      (let inner ((lhs (cdar lst))
                  (out out))
        (cond
          ((null? lhs)
           (loop (cdr lst) out))
          ((not (plan? (car lhs)))
           (inner (cdr lhs) out))
          ((plan-built? (car lhs))
           (inner (cdr lhs) out))
          (else
            (inner (cdr lhs)
                   (cons (proc (car lhs)) out))))))))

(: unbuilt-dfs (((struct plan) -> *) (struct plan) -> *))
(define (unbuilt-dfs proc p)
  (plan-dfs
    (lambda (p)
      (and (plan? p)
           (or (plan-built? p)
               (proc p))))
    p))

;; compute-stages assigns stages to the build,
;; where leaf nodes of the DAG are stage 0,
;; their dependents are stage 1, and so forth
(: compute-stages ((list-of (struct plan)) -> vector))
(define (compute-stages lst)
  (let ((stage (make-hash-table test: eq? hash: eq?-hash)))
    (define (stage-of p)
      ;; a plan's stage is 1+(greatest input stage),
      ;; where a plan with no other inputs is stage 0
      (+ 1
         ((input-seq p)
          (k/map (cut hash-table-ref stage <>) max)
          -1)))
    (for-each
      (lambda (p)
        (unbuilt-dfs
          (lambda (p)
            (or (hash-table-ref/default stage p #f)
                (let ((val (stage-of p)))
                  (hash-table-set! stage p val))))
          p))
      lst)
    (let* ((endstage ((list->seq lst) (k/map (cut hash-table-ref/default stage <> -1) max) -1))
           (vec      (make-vector (+ endstage 1) '())))
      (hash-table-for-each
        stage
        (lambda (p stage)
          (vector-set! vec stage (cons p (vector-ref vec stage)))))
      vec)))

(: build-graph! ((list-of (struct plan)) #!rest * -> *))
(define (build-graph! lst #!key (maxprocs (nproc)))
  (let ((sema   (make-semaphore maxprocs))
        (stages (compute-stages lst))
        (ht     (make-hash-table test: eq? hash: eq?-hash))
        (err    #f))
    ;; core build procedure:
    ;; first, wait for inputs (dependencies) to complete,
    ;; then either a) return, if there has been an error,
    ;; or b) acquire up to maxjobs jobs from the semaphore
    ;; and then execute with that level of intra-build parallelism
    (define (build-one! p maxjobs)
      (let loop ((inputs (input-map (cut hash-table-ref ht <>) p)))
        (if (null? inputs)
          ;; by the time all the inputs are built,
          ;; we may *discover* that this plan is identical
          ;; to one that is already built; in that case, bail
          (if (plan-built? p)
            (plan-outputs p)
            (let ((jobs (semacquire/max sema (if (plan-parallel p) maxjobs 1))))
              (or err
                  (parameterize ((info-prefix (string-append (plan-name p) " j=" (number->string jobs) " |")))
                    (infoln "building")
                    (save-plan-outputs! p (plan->outputs! p jobs))
                    (infoln "completed")))
              (semrelease/n sema jobs)
              #t))
          (let ((v (join/value (car inputs))))
            (cond
              (err err)
              ((condition? v) (begin (set! err v) v))
              (else (loop (cdr inputs))))))))
    ;; spawn a builder coroutine for each unbuilt package
    (let loop ((i 0))
      (or (>= i (vector-length stages))
          (let* ((stage (vector-ref stages i))
                 (slen  (length stage))
                 (maxp  (max 1 (quotient maxprocs (max slen 1)))))
            ;; spawn builds for each stage
            (for-each
              (lambda (p)
                (hash-table-set! ht p (spawn build-one! p maxp)))
              stage)
            (loop (+ i 1)))))
    ;; join the arguments
    (let loop ((lst lst))
      (or (null? lst)
          (let ((proc (hash-table-ref/default ht (car lst) #f)))
            (when proc
              (join/value proc))
            (loop (cdr lst)))))
    (if err (abort err) #t)))

;; build-plan! unconditionally builds a plan
;; and produces its output artifact
;; (it does not save the output)
(: build-plan! ((struct plan) -> artifact))
(define (build-plan! top)
  (unless (plan-resolved? top)
    (error "called build-plan! on unresolved plan"))
  (infoln "building" (plan-name top))
  (plan->outputs! top (nproc)))

;; load-plan loads an old plan from the plan directory
(: load-plan (string -> (struct plan)))
(define (load-plan hash)
  (let* ((label   (with-input-from-file
                    (filepath-join (plan-dir) hash "label")
                    read-string))
         (vinput  (with-input-from-file
                    (filepath-join (plan-dir) hash "inputs.scm")
                    read))
         (vec->in (lambda (v)
                    (cons (vector-ref v 0)
                          (list
                            (vector
                              (vector-ref v 1)
                              (vector-ref v 2)
                              #f)))))
         (inputs  (map vec->in vinput))
         (plan    (make-plan
                    name:   label
                    inputs: inputs))
         (newhash (plan-hash plan)))
    (unless (string=? newhash hash)
      (error "loaded plan has different hash:" newhash))
    plan))
