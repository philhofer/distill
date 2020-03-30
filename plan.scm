(foreign-declare "#include \"copy-sparse.c\"")

;; we have our own copy-file that preserves sparse files
;; and can optimizes some copies into a rename(2)
(: copy-file (string string boolean -> true))
(define (copy-file src dst rename?)
  (let* ((raw (foreign-lambda
               int "copy_file_sparse"
               nonnull-c-string nonnull-c-string bool))
         (e   (raw src dst rename?)))
    (or (= e 0)
        (error "copy_file_sparse: errno" e))))

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

(define (interned-dir abspath mode)
  (%artifact
    `#(dir ,abspath ,mode)
    (hash-string abspath)
    #f))

(: overlay (string -> vector))
(define (overlay abspath)
  (let* ((p (filepath-join (current-directory) abspath))
         (h (hash-file p)))
    (unless h
      (error "overlay file doesn't exist:" p))
    (let ((dst (filepath-join (artifact-dir) h)))
      (unless (file-exists? dst)
        (copy-file p dst #f)))
    (%artifact
      `#(file ,abspath ,(file-permissions p))
      h
      #f)))

;; by default, dump stuff into these directories
;; TODO: inherit these from the environment
(define plan-dir (make-parameter "./plans"))
(define artifact-dir (make-parameter "./artifacts"))

(: artifact-path (vector -> string))
(define (artifact-path art)
  (filepath-join (artifact-dir) (artifact-hash art)))

;; plan is the lowest-level representation of a "package"
;; or other build step; it simply connects itself to other
;; inputs plus a recipe for producing the output
;;
;; essentially, packages are "compiled" as
;; package-lambda -> package -> plan
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
(define <plan>
  (make-kvector-type
    name:
    inputs:
    saved-hash:
    saved-output:
    raw-output:))

(define plan? (kvector-predicate <plan>))

(define make-plan
  (kvector-constructor
    <plan>
    name:         #f string? ;; TODO: validate name further?
    inputs:       '() (list-of
                        (pair-of string? (list-of (or/c artifact? plan?))))
    saved-hash:   #f false/c
    saved-output: #f false/c
    raw-output:   #f (or/c string? false/c)))

(: plan-saved-hash (vector --> (or string false)))
(define plan-saved-hash (kvector-getter <plan> saved-hash:))
(: plan-inputs (vector --> (list-of (pair string (list-of vector)))))
(define plan-inputs (kvector-getter <plan> inputs:))
(: plan-name (vector --> string))
(define plan-name (kvector-getter <plan> name:))
(: plan-saved-output (vector --> (or false vector)))
(define plan-saved-output (kvector-getter <plan> saved-output:))
(: plan-raw-output (vector --> (or false string)))
(define plan-raw-output (kvector-getter <plan> raw-output:))

(define plan-saved-hash-set! (kvector-setter <plan> saved-hash:))
(define plan-saved-output-set! (kvector-setter <plan> saved-output:))

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
(: plan-resolved? (vector -> boolean))
(define (plan-resolved? p)
  (all/s? (lambda (in)
            (or (artifact? in)
                (plan-outputs in)))
          (input-seq p)))

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
  (let* ((inputs ((list->seq inputs) ((k/map pair->inseq) (k/recur cons)) '()))
         (inputs (sort inputs art<?)))
    (write inputs out)))

;; plan-hash returns the canonical hash of a plan,
;; or #f if any of its inputs have unknown outputs
(: plan-hash (vector -> (or false string)))
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
               (call-with-output-file ofd (cut write-string str #f <>))
               (call-with-output-file lfd (cute display (plan-name p) <>)))
             (plan-saved-hash-set! p h)
             h))))

;; plan-exn wraps a srfi-12 condition
;; with another srfi-12 condition that is tagged
;; with 'plan
(define (plan-exn plan suberr)
  (make-property-condition
    'plan-failure
    'plan  plan
    'child suberr))

;; handle a fatal plan failure by printing diagnostics
;; and exiting with a non-zero status
(define (fatal-plan-failure exn)
  ;; if this function throws for some reason, don't get caught in a loop
  (parameterize ((current-exception-handler
                   (lambda (exn)
                     (display "exception in exception handler!\n" (current-error-port))
                     (display exn (current-error-port))
                     (exit 1))))
    (let* ((sys?   (condition-predicate 'exn))
           (eplan? (condition-predicate 'plan-failure)))
      (cond
        ((sys? exn)
         (let ((chain (condition-property-accessor 'exn 'call-chain))
               (eport (current-error-port)))
           (print-error-message exn)
           (let ((lst (chain exn)))
             (for-each
               (lambda (v)
                 (display (vector-ref v 0) eport)
                 (newline eport))
               lst))
           (fatal "exited due to uncaught exception.")))
        ((eplan? exn)
         (let* ((prop  (lambda (sym) (condition-property-accessor 'plan-failure sym)))
                (plan  ((prop 'plan) exn))
                (child ((prop 'child) exn)))
           (info "plan" (plan-name plan) "encountered a fatal error:")
           (fatal-plan-failure child)))
        (else (abort exn))))))

;; fork+exec, wait for the process to exit and check
;; that it exited successfully
(: run (string #!rest string -> undefined))
(define (run prog . args)
  (trace "run" (cons prog args))
  (let-values (((pid ok? status) (process-wait/yield (process-run prog args))))
    (unless (and ok? (= status 0))
      (error "command failed:" (cons prog args)))))

(define current-jobserver
  (make-parameter #f))

;; call a thunk with a fresh jobserver bound to current-jobserver
;; during the dynamic extent of the thunk
(: with-new-jobserver ((-> 'a) -> 'a))
(define (with-new-jobserver thunk)
  (let* ((js (fdpipe))
         (res (parameterize ((current-jobserver js))
                (thunk))))
    (fdclose (car js))
    (fdclose (cadr js))
    res))

(: jobserver+ (fixnum -> *))
(define (jobserver+ n)
  (let* ((pipe (current-jobserver))
         (wfd  (cadr pipe))
         (bv   (make-string n #\a)))
    (fdwrite wfd bv)))

(: jobserver- (fixnum -> *))
(define (jobserver- n)
  (or (fx<= n 0)
      (let* ((rfd   (car (current-jobserver)))
             (buf   (make-string n))
             (ret   (fdread rfd buf)))
        (cond
          ((fx= ret 0) (error "pipe: EOF"))
          ((fx< ret 0) (error "errno:" (- ret)))
          (else        (jobserver- (fx- n ret)))))))

(: call-with-job ((-> 'a) -> 'a))
(define (call-with-job proc)
  (let ((throw (current-exception-handler)))
    (jobserver- 1)
    (let ((res
            (parameterize ((current-exception-handler
                             (lambda (exn)
                               (current-exception-handler throw)
                               (jobserver+ 1)
                               (throw exn))))
              (proc))))
      (jobserver+ 1)
      res)))

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
    (infoln "fetching" url)
    (run "wget" "-q" "-O" tmp url)
    (let ((h (hash-file tmp)))
      (if (string=? h hash)
        (rename-file tmp dst #t)
        (begin
          (delete-file tmp)
          (fatal "fetched artifact has the wrong hash:" h "not" hash))))))

;; plan-outputs-file is the file that stores the serialized
;; interned file information for a plan
(: plan-outputs-file (vector -> (or string false)))
(define (plan-outputs-file p)
  (and-let* ((h (plan-hash p)))
    (filepath-join (plan-dir) h "outputs.scm")))

;; write 'lst' as the set of plan outputs associated with 'p'
(: save-plan-outputs! (vector artifact -> *))
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
(: plan-outputs (vector -> (or artifact false)))
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
      (copy-file (filepath-join (artifact-dir) hash) dstfile #f)
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

  (define (unpack-dir dst abspath mode)
    (let ((dir (filepath-join dst abspath)))
      (when (and (directory-exists? dir)
                 (not (= mode (file-permissions dir))))
        (error "directory exists but has conflicting permissions:" dir))
      (create-directory dir #t)
      (set-file-permissions! dir mode)))

  (let ((format (artifact-format i))
        (hash   (artifact-hash i))
        (extra  (artifact-extra i)))
    (match format
      (#('file abspath mode)   (unpack-file dst abspath mode hash extra))
      (#('dir abspath mode)    (unpack-dir dst abspath mode))
      (#('archive kind)        (unpack-archive dst kind hash extra))
      (#('symlink abspath lnk) (unpack-symlink dst abspath lnk))
      (else (error "unrecognized artifact format" format)))))

(: with-tmpdir (forall (a) ((string -> a) -> a)))
(define (with-tmpdir proc)
  (let* ((dir (create-temporary-directory))
         (_   (set-file-permissions! dir #o700))
         (res (proc dir)))
    (delete-directory dir #t)
    res))

;; intern! interns a file into the artifact directory
;; and returns its hash;
;; the interning operation may rename the file,
;; or it will copy the file while preserving holes
(: intern! (string -> string))
(define (intern! fp)
  (let ((h (hash-file fp)))
    (unless h (error "couldn't find file" fp))
    (let ((dst (filepath-join (artifact-dir) h)))
      (if (and (file-exists? dst) (equal? (hash-file dst) h))
        (infoln "artifact reproduced:" (short-hash h))
        (begin
          (copy-file fp dst #t)
          ;; regardless of source file permissions,
          ;; artifacts should have 644 perms
          (set-file-permissions! dst #o644)))
      h)))

(: file->artifact (string string -> artifact))
(define (file->artifact f abspath)
  (let ((perm (file-permissions f))
        (h    (intern! f)))
    (vector
      `#(file ,abspath ,perm)
      h
      #f)))

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
    (local-archive format (intern! tmp))))

;; plan->outputs! builds a plan and yields
;; the list of interned file handles that are installed
;; into the /out directory in the jail
;;
;; all of the input plan's dependencies must
;; have been built (i.e. have plan-outputs)
;; in order for this to work
(: plan->outputs! (vector -> artifact))
(define (plan->outputs! p)
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
      (let* ((pipefd (current-jobserver))
             (infd   (car pipefd))
             (outfd  (cadr pipefd)))
        (apply
          sandbox-run
          root
          (list
            "/bin/emptyenv"
            "/bin/envfile" "/env"
            ;; close stdin and redirect stdin+stderr to a log file
            ;; (otherwise builds just get really noisy in the terminal)
            "redirfd" "-r" "0" "/dev/null"
            "redirfd" "-w" "1" "/build.log"
            "fdmove" "-c" "2" "1"
            ;; TODO: seriously broken build scripts
            ;; may not be reproducible if the environment
            ;; is not 100% identical...
            "export" "MAKEFLAGS" (string-append
                                   "--jobserver-auth="
                                   (number->string infd)
                                   ","
                                   (number->string outfd))
            "/build")))

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
      (let ((raw (plan-raw-output p)))
        (if raw
          (file->artifact (filepath-join outdir raw) raw)
          (dir->artifact outdir))))))

(: plan-built? (vector -> boolean))
(define (plan-built? p)
  (if (plan-outputs p) #t #f))

;; k/unbuilt is a reducer-transformer
;; that yields only unbuilt plans to its reducer
(define k/unbuilt
  (let ((unbuilt? (lambda (in)
                    (and (plan? in) (not (plan-built? in))))))
    (k/filter unbuilt?)))

;; k/unbuilt-dfs is a reducer-transformer
;; that walks unbuilt plans in DFS order
(define k/unbuilt-dfs
  (kompose
    (label top)
    (k/uniq test: eq? hash: eq?-hash)
    k/unbuilt
    (k/postorder top input-seq)))

;; for-each/unbuilt-dfs walks a list of plans
;; in depth-first order and calls (proc p) on
;; each plan that is unbuilt
(: for-each/unbuilt-dfs ((vector -> *) (list-of vector) -> undefined))
(define (for-each/unbuilt-dfs proc pl)
  ((list->seq pl)
   (k/unbuilt-dfs
     (lambda (in out)
       (proc in)
       out))
   (void)))

(: build-graph! ((list-of vector) #!rest * -> *))
(define (build-graph! lst #!key (maxprocs (nproc)))
  (let* ((ht             (make-hash-table test: eq? hash: eq?-hash))
         (err            #f)
         ;; join-dep takes a plan input and
         ;; waits for it to complete (if it is a plan),
         ;; and returns #t if the input was built successfully
         ;; or #f otherwise
         (join-dep       (lambda (in)
                           (cond
                             ((hash-table-ref/default ht in #f)
                              => (lambda (p)
                                   (let ((ret (join/value p)))
                                     (and (not err)
                                          (if (eq? (proc-status p) 'exn)
                                            (begin (set! err ret) #f)
                                            #t)))))
                             ((and (plan? in) (not (plan-built? in)))
                              (error "input plan unbuild but not scheduled" (plan-name in)))
                             (else (not err)))))
         ;; should-build? waits for all of the
         ;; input plans for 'p' to be done building
         ;; and then checks if we should really build
         ;; (i.e. no error has been encountered and
         ;; the plan is actually stale)
         (should-build?  (lambda (p)
                           (and (all/s? join-dep (input-seq p))
                                (not (plan-built? p))))))
    ;; core build procedure:
    ;; first, wait for inputs (dependencies) to complete,
    ;; then either a) return, if there has been an error,
    ;; or b) acquire a job from the jobserver and start running
    (define (build-one! p)
      (let ((rethrow (current-exception-handler)))
        ;; if this build encounters an error, catch it and re-throw
        ;; with associated plan information (should aid debugging)
        (parameterize ((info-prefix (string-append (plan-name p) " |"))
                       (current-exception-handler (lambda (exn)
                                                    (rethrow
                                                      (plan-exn p exn)))))
          (if (should-build? p)
            ;; in the time spent waiting to acquire resources,
            ;; another plan could have encountered an error, so
            ;; this inner bit should become a no-op in that situation
            (or err
                (call-with-job
                  (lambda ()
                    (infoln "building")
                    (save-plan-outputs! p (plan->outputs! p))
                    (infoln "completed"))))
            #t))))
    ;; spawn a builder coroutine for each unbuilt package
    (with-new-jobserver
      (lambda ()
        (jobserver+ maxprocs)
        (for-each/unbuilt-dfs
          (lambda (p)
            (when (hash-table-ref/default ht p #f)
              (error "plan already present?" (plan-name p)))
            (hash-table-set! ht p (spawn build-one! p)))
          lst)
        ;; wait for every coroutine to exit; the jobserver
        ;; must live at least this long
        (for-each
          (lambda (p)
            (let ((ret (join/value p)))
              (if (eq? (proc-status p) 'exn)
                (or err (set! err ret)))))
          (hash-table-values ht))))
    (if err (fatal-plan-failure err) #t)))

;; build-plan! unconditionally builds a plan
;; and produces its output artifact
;; (it does not save the output)
(: build-plan! (vector -> artifact))
(define (build-plan! top)
  (unless (plan-resolved? top)
    (error "called build-plan! on unresolved plan"))
  (infoln "building" (plan-name top))
  (with-new-jobserver
    (lambda ()
      (let ((np (nproc)))
        (when (> np 1)
          (jobserver+ (- np 1)))
        (plan->outputs! top)))))

;; load-plan loads an old plan from the plan directory
;;
;; the plan *must* have a label and inputs, and *may* have outputs
;; (however, re-building a plan without knowing its outputs may
;; lead to the outputs being serialized differently than another
;; build, which could lead to spurious reproducibility issues)
(: load-plan (string -> vector))
(define (load-plan hash)
  (let* ((label   (with-input-from-file
                    (filepath-join (plan-dir) hash "label")
                    read-string))
         (vinput  (with-input-from-file
                    (filepath-join (plan-dir) hash "inputs.scm")
                    read))
         (voutput (let ((fp (filepath-join (plan-dir) hash "outputs.scm")))
                    (if (file-exists? fp)
                      (with-input-from-file fp read)
                      #f)))
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
                    inputs: inputs
                    saved-output: voutput
                    ;; if the original build had a raw output, then
                    ;; use the same output file name
                    ;; to retain reproducibility
                    raw-output: (and voutput
                                  (let ((format (artifact-format voutput)))
                                    (and (eq? (vector-ref format 0) 'file) (vector-ref format 1))))))
         (newhash (plan-hash plan)))
    (unless (string=? newhash hash)
      (error "loaded plan has different hash:" newhash))
    plan))
