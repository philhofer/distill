(foreign-declare "#include <fnmatch.h>")
(foreign-declare "#include \"copy-sparse.c\"")

;; we have our own copy-file that preserves sparse files
;; and can optimizes some copies into a rename(2)
(: copy-file (string string boolean -> true))
(define (copy-file src dst rename?)
  (let* ((raw (foreign-lambda int "copy_file_sparse"
                              nonnull-c-string nonnull-c-string bool))
         (e   (raw src dst rename?)))
    (or (= e 0)
        (error "copy_file_sparse: errno" src dst rename? e))))

;; Artifacts are just vectors
;;
;; note that artifact-extra must not contain
;; data that changes the filesystem representation
;; of the artifact (it is ignored for plan hashing purposes)
(define-type artifact (vector vector string *))
(: %artifact (forall (a) (vector (or string procedure) a --> (vector vector (or string procedure) a))))
(define (%artifact format hash extra)
  (vector format hash extra))

(: artifact-format (artifact -> vector))
(define (artifact-format v) (vector-ref v 0))

(: artifact-hash (artifact -> string))
(define (artifact-hash v)
  (let ((h (vector-ref v 1)))
    (cond
     ((string? h) h)
     ((procedure? h)
      (let ((res (h)))
        (vector-set! v 1 res)
        res))
     (else (error "unexpected value in artifact-hash slot" v)))))

(: artifact-extra (artifact -> *))
(define (artifact-extra v) (vector-ref v 2))

(: artifact? (* -> boolean))
(define (artifact? v) (and (vector? v) (= (vector-length v) 3)))

(: artifact-kind (artifact -> symbol))
(define (artifact-kind v)
  (vector-ref (artifact-format v) 0))

;; short-hash is a 6-character hash prefix
(define (short-hash h)
  (let ((n (string-length h)))
    (if (> n 6)
        (##sys#substring h 0 6)
        (error "bad argument to short-hash" h))))

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
                     (".txz"    . tar.xz)
                     (".tar.gz" . tar.gz)
                     (".tgz"    . tar.gz)
                     (".tar.bz" . tar.bz)
                     (".tar.bz2". tar.bz)
                     (".tar.zst". tar.zst)
                     (".tar"    . tar))))
    (cond
     ((null? suff)
      ;; hack because some package sources are stored in the CDN,
      ;; thus the url does not contain an extension
      (if (string-prefix? "https://b2cdn.sunfi.sh" src)
          'tar.zst
          (error "bad archive suffix" src)))
     ((string-suffix? (caar suff) src)
      (cdar suff))
     (else (loop (cdr suff))))))

;; TODO: normalize remote archives for faster re-builds
;; (large xz tarballs are reeeeally slow to unpack)
(: remote-archive (string string #!rest * --> artifact))
(define (remote-archive src hash #!key (kind #f))
  (%artifact
   `#(archive ,(or kind (impute-format src)))
   hash
   src))

(: sub-archive (artifact list --> artifact))
(define (sub-archive art args)
  (%artifact
   `#(sub-archive ,(vector-ref (artifact-format art) 1) ,args)
   (artifact-hash art)
   #f))

;; subarchive-match takes an archive artifact
;; and a list of glob expressions and returns
;; the list of archive members that match 'matches'
(: archive-match (string (list-of string) -> (list-of string)))
(define (archive-match file matches)
  (unless (file-exists? file)
    (error "archive-match: file doesn't exist" file))
  (let* ((fnmatch (foreign-lambda* int ((nonnull-c-string pat) (nonnull-c-string str))
                                   "C_return(fnmatch(pat, str, FNM_PATHNAME));"))
         (matches? (lambda (item)
                     (let loop ((exprs matches))
                       (if (null? exprs)
                           #f
                           (or (= 0 (fnmatch (car exprs) item))
                               (loop (cdr exprs))))))))
    (let-values (((rfd wfd) (create-pipe)))
      (let* ((child (process-fork
                     (lambda ()
                       (fdclose rfd)
                       (duplicate-fileno wfd 1)
                       (fdclose wfd)
                       (process-execute
                        "env"
                        (list "LC_ALL=C.UTF-8" "tar" "-tf" file)))))
             (end   (spawn process-wait/yield child))
             (inp   (open-input-file* rfd)))
        (fdclose wfd)
        (with-cleanup
         (lambda ()
           (begin
             (fdclose rfd)
             (join/value end)))
         (lambda ()
           (let loop ((out  '())
                      (line  (read-line inp)))
             (if (eof-object? line)
                 out
                 (loop
                  (if (matches? line) (cons line out) out)
                  (read-line inp))))))))))

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

(: interned (string integer (or string procedure) -> artifact))
(define (interned abspath mode contents)
  (let ((kind `#(file ,abspath ,mode)))
    (cond
     ((string? contents)
      (%artifact kind (lambda () (hash-of contents)) (cons 'inline contents)))
     ((procedure? contents)
      (%artifact kind (lambda () (with-interned-output contents)) #f))
     (else (error "bad argument to plan#interned")))))

;; interned-symlink creates a link at 'abspath'
;; that points to 'lnk'
(: interned-symlink (string string --> vector))
(define (interned-symlink abspath lnk)
  (%artifact
   `#(symlink ,abspath ,lnk)
   (hash-of lnk)
   #f))

(: interned-dir (string integer -> vector))
(define (interned-dir abspath mode)
  (%artifact
   `#(dir ,abspath ,mode)
   (hash-of abspath)
   #f))

(: overlay (string string -> vector))
(define (overlay herepath therepath)
  (%artifact
   `#(file ,therepath ,(file-permissions herepath))
   (lambda ()
     (let ((h (hash-file herepath)))
       (unless h (error "overlay file doesn't exist" herepath))
       (let ((dst (filepath-join (artifact-dir) h)))
         (or (file-exists? dst)
             (copy-file herepath dst #f))
         h)))
   (cons 'local herepath)))

;; by default, dump stuff into these directories
(define plan-dir
  (make-parameter (or (get-environment-variable "DISTILL_PLAN_DIR") "./plans")))
(define artifact-dir
  (make-parameter (or (get-environment-variable "DISTILL_ARTIFACT_DIR") "./artifacts")))

(: artifact-path (vector -> string))
(define (artifact-path art)
  (filepath-join (artifact-dir) (artifact-hash art)))

(define-kvector-type
  <input>
  make-input
  input?
  (input-basedir basedir: "/" string?)
  (input-link    link:    #f  vector?)
  (input-wrap    wrap:    #f  (perhaps procedure?)))

(define input-set-link! (kvector-setter <input> link:))
(define input-set-wrap! (kvector-setter <input> wrap:))

(define-kvector-type
  <plan>
  make-plan
  plan?
  (plan-name   name:   #f  string?)
  (plan-inputs inputs: '() (list-of input?))
  (plan-saved-hash   saved-hash:   #f false/c)
  (plan-saved-output saved-output: #f false/c)
  (plan-null-build?  null-build:   #f boolean?)
  (plan-raw-output   raw-output:   #f (perhaps string?)))

(define plan-saved-hash-set! (kvector-setter <plan> saved-hash:))
(define plan-saved-output-set! (kvector-setter <plan> saved-output:))

;; input-resolve! returns the artifact associated with
;; an input, or #f if it points to a plan with unknown outputs
(: input-resolve! (vector -> (or vector false)))
(define (input-resolve! in)
  (let ((link (input-link in)))
    (cond
     ((artifact? link)
      link)
     ((plan? link)
      (let ((art (plan-outputs link)))
        (and art
             (let* ((wr  (or (input-wrap in) identity))
                    (val (wr art)))
               (input-set-link! in val)
               (input-set-wrap! in #f)
               val))))
     (else (error "unexpected <input> link value" link)))))

(: fold-unresolved (vector procedure * -> *))
(define (fold-unresolved plan proc seed)
  (let loop ((out seed)
             (lst (plan-inputs plan)))
    (if (null? lst)
        out
        (let ((head (car lst)))
          (loop
           (if (input-resolve! head)
               out
               (proc (input-link head) out))
           (cdr lst))))))

(: map-unresolved (vector (vector -> *) -> *))
(define (map-unresolved plan proc)
  (fold-unresolved
   plan
   (lambda (inplan lst)
     (cons (proc inplan) lst))
   '()))

(: foldl1 (procedure * list -> *))
(define (foldl1 proc init lst)
  (let loop ((out init)
             (lst lst))
    (if (null? lst)
        out
        (loop (proc (car lst) out) (cdr lst)))))

(define (fold-graph proc init headlst)
  (foldl1
   (lambda (item val)
     (proc item (if (plan? item)
                    (foldl1
                     (lambda (in val)
                       (proc (input-link in) val))
                     val (plan-inputs item))
                    val)))
   init
   headlst))

(: andmap1 (procedure list -> *))
(define (andmap1 pred? lst)
  (let loop ((lst lst))
    (or (null? lst)
        (and (pred? (car lst))
             (loop (cdr lst))))))

;; not defined in R7RS, but trivial enough to implement
(: call-with-output-string ((output-port -> *) -> string))
(define (call-with-output-string proc)
  (let ((p (open-output-string)))
    (proc p)
    (let ((s (get-output-string p)))
      (close-output-port p)
      s)))

;; with-interned-output calls a thunk
;; and interns everything written to
;; current-output-port into the current
;; artifact directory
(: with-interned-output ((-> *) -> string))
(define (with-interned-output thunk)
  (let* ((_  (check-tmp-perms))
         (h  (new-hasher))
         (f  (create-temporary-file ".to-intern"))
         (fp (open-output-file f))
         (bp (make-broadcast-port (hasher->output-port h) fp)))
    (parameterize ((current-output-port bp))
      (thunk))
    (close-output-port bp)
    (close-output-port fp)
    (let* ((hres (hash-finalize h))
           (dst  (filepath-join (artifact-dir) hres)))
      (or (file-exists? dst)
          (copy-file f dst #t))
      (delete-file* f)
      hres)))

;; plan-resolved? returns whether or not
;; all of the inputs to this plan are resolved
;; (i.e. artifacts or plans with known outputs)
(: plan-resolved? (vector -> boolean))
(define (plan-resolved? p)
  (andmap1 input-resolve! (plan-inputs p)))

;; canonicalize takes plan-inputs and
;; produces those inputs in canonicalized form
;; (used for calculating plan hashes)
;;
;; note that input-resolve! is called on the inputs
;; as part of the canonicalization process
(: canonicalize ((list-of vector) -> (list-of vector)))
(define (canonicalize! inputs)

  ;; turn an input into an artifact unconditionally
  (: ->repr (vector -> vector))
  (define (->repr in)
    (let ((art (input-resolve! in)))
      (unless (artifact? art)
        (error "expected an artifact; got" art (plan-name (input-link in))))
      (artifact-repr (input-basedir in) art)))

  ;; sort input artifacts by extraction directory,
  ;; then by content hash
  (: art<? ((vector string vector string) --> boolean))
  (define (art<? a b)
    (let ((aroot (vector-ref a 0))
          (broot (vector-ref b 0))
          (ahash (vector-ref a 2))
          (bhash (vector-ref b 2)))
      (or (string<? aroot broot)
          (and (string=? aroot broot)
               (string<? ahash bhash)))))

  ;; write plans in a format that can be deserialized;
  ;; that way we can build old plans even if we don't
  ;; have the code that generated them (as long as
  ;; we have the original artifacts)
  (sort (map ->repr inputs) art<?))

;; check-tmp-perms performs a check (once)
;; that create-temporary-file won't create files
;; in a world-writable directory
(define check-tmp-perms
  (let ((self
         (delay
           (let* ((f    (create-temporary-file ".check"))
                  (dir  (dirname f))
                  (perm (file-permissions dir)))
             (unless (or (not (= (bitwise-and perm perm/isvtx) 0))
                         (= (bitwise-and perm perm/iwoth) 0))
               (fatal "please set $TMP to a directory that is sticky or not world-writable"))
             (delete-file* f)
             #t))))
    (lambda () (force self))))

;; plan-hash returns the canonical hash of a plan,
;; or #f if any of its inputs have unknown outputs
(: plan-hash (vector -> (or false string)))
(define (plan-hash p)
  (or (plan-saved-hash p)
      (and (plan-resolved? p)
           (let* ((h   (with-interned-output
                        (lambda ()
                          (write (canonicalize! (plan-inputs p))))))
                  (dir (filepath-join (plan-dir) h))
                  (lfd (string-append dir "/label")))
             (unless (file-exists? lfd)
               (create-directory dir #t)
               (call-with-output-file
                   lfd
                 (cute display (string-append (plan-name p) "-" (short-hash h)) <>)))
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
       (else (begin
               (print-error-message exn)
               (fatal "fatal error; exited")))))))

;; fork+exec, wait for the process to exit and check
;; that it exited successfully
(: run (string (list-of string) -> undefined))
(define (run prog args)
  (let-values (((pid ok? status) (process-wait/yield (process-run prog args))))
    (unless (and ok? (= status 0))
      (error "command failed:" (cons prog args)))))

(define current-jobserver
  (make-parameter #f))

;; call a thunk with a fresh jobserver bound to current-jobserver
;; during the dynamic extent of the thunk
(: with-new-jobserver ((-> 'a) -> 'a))
(define (with-new-jobserver thunk)
  (let ((js (fdpipe)))
    (with-cleanup
     (lambda ()
       (fdclose (car js))
       (fdclose (cadr js)))
     (lambda ()
       (parameterize ((current-jobserver js))
         (thunk))))))

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
  (jobserver- 1)
  (with-cleanup
   (lambda () (jobserver+ 1))
   proc))

;; perform an elaborate chroot into 'root'
;; and then run '/build' inside that new
;; root, with stdout and stderr redirected
;; to 'logfile' (either a file path or file descriptor)
(: sandbox-run (string string -> undefined))
(define (sandbox-run root logfile)
  (let ((bwrap "/usr/bin/bwrap") ;; FIXME
        (js    (current-jobserver))
        (args  (list
                "--unshare-ipc"
                "--unshare-pid"
                "--unshare-uts"
                "--unshare-cgroup-try"
                "--unshare-net"
                "--hostname" "builder"
                "--bind" root "/"       ; rootfs containing host tools
                "--dir" "/dev"
                "--dir" "/proc"
                "--dir" "/tmp"
                "--dev" "/dev"
                "--proc" "/proc"
                "--tmpfs" "/tmp"
                "--"
                "/build"))
        ;; DO NOT CHANGE THIS LIGHTLY:
        ;; it may cause builds to fail
        ;; to reproduce!
        (env   '(("PATH" . "/bin:/sbin:/usr/bin:/usr/sbin:/usr/local/bin")
                 ("LC_ALL" . "C.UTF-8")
                 ("SOURCE_DATE_EPOCH" . "0")
                 ("MAKEFLAGS" . "--jobserver-auth=5,6")))
        (setfd! (lambda (fd fromfd)
                  (or (= fd fromfd)
                      (begin
                        (duplicate-fileno fromfd fd)
                        (file-close fromfd))))))
    (let-values (((pid ok status)
                  (process-wait/yield
                   (process-fork
                    (lambda ()
                      ;; any exceptions in here should immediately exit
                      (current-exception-handler (lambda (exn)
                                                   (print-error-message exn)
                                                   (fatal "(execing bwrap):" exn)))
                      (setfd! 6 (cadr js))
                      (setfd! 5 (car js))
                      (setfd! fileno/stdin (file-open "/dev/null" open/rdonly))
                      ;; can't use fdpipe here because we need a *blocking* pipe;
                      (let-values (((rd wr) (create-pipe)))
                        (process-fork
                         (lambda ()
                           (current-exception-handler
                            (lambda (exn)
                              (print-error-message exn)
                              (fatal "(execing zstd):" exn)))
                           (for-each file-close '(5 6))
                           (file-close wr)
                           (setfd! fileno/stdin rd)
                           (process-execute "zstd" (list "-q" "-" "-o" logfile))))
                        (file-close rd)
                        (setfd! fileno/stdout wr))
                      (duplicate-fileno fileno/stdout fileno/stderr)
                      (process-execute bwrap args env))))))
      (or (and ok (= status 0))
          (error "sandbox build failed")))))

(: fetch! ((or false string) string -> *))
(define (fetch! src hash)
  (let* ((url (or src (begin
                        (infoln "trying to fetch" (short-hash hash) "from fallback CDN")
                        (string-append (cdn-url) hash))))
         (dst (filepath-join (artifact-dir) hash))
         (tmp (string-append dst ".tmp")))
    (infoln "fetching" url)
    (let ((h (fetch+hash url tmp)))
      (cond
       ((not h)
        (error "fetching url failed" url))
       ((string=? h hash)
        (rename-file tmp dst #t))
       (else
        (begin
          (delete-file tmp)
          (error "fetched artifact has the wrong hash" h "not" hash)))))))

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
    (let ((old    (plan-outputs p))
          (outdir (dirname outfile))
          (outlnk (filepath-join (plan-dir) (string-append "output:" (artifact-hash ar)))))
      (cond
       ((not old)
        (begin
          ;; only keep the 'most recent' plan that
          ;; produces a particular output
          (when (file-exists? outlnk)
            (delete-file outlnk))
          (create-symbolic-link (plan-hash p) outlnk)
          (create-directory outdir)
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

(define cdn-url
  (make-parameter "https://b2cdn.sunfi.sh/file/pub-cdn/"))

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
      (when (file-exists? dstfile)
        (error "cannot unpack file (it already exists)" dstfile))
      (create-directory (dirname dstfile) #t)
      (copy-file (filepath-join (artifact-dir) hash) dstfile #f)
      (set-file-permissions! dstfile mode)))

  (define (unpack-sub-archive dst kind hash src args)
    (unpack-archive dst kind hash src
                    (archive-match
                     (filepath-join (artifact-dir) hash)
                     args)))

  (define (unpack-archive dst kind hash src tail)
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
      (run "tar" (cons* comp "-xkf" infile "-C" dst tail))))

  (define (unpack-symlink dst abspath lnk)
    (let ((target (filepath-join dst abspath)))
      (when (file-exists? target)
        (error "conflicting symlink for" abspath))
      (create-directory (dirname target) #t)
      (create-symbolic-link lnk target)))

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
           (#('archive kind)        (unpack-archive dst kind hash extra '()))
           (#('symlink abspath lnk) (unpack-symlink dst abspath lnk))
           (#('sub-archive kind tail) (unpack-sub-archive dst kind hash extra tail))
           (else (error "unrecognized artifact format" format)))))

(: with-tmpdir (forall (a) ((string -> a) -> a)))
(define (with-tmpdir proc)
  (let ((_   (check-tmp-perms))
        (dir (create-temporary-directory)))
    (set-file-permissions! dir #o700)
    (with-cleanup
     (lambda () (delete-directory dir #t))
     (lambda () (proc dir)))))

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
      (delete-file* fp))
    h))

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
     (list
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
      "-C" dir "."))
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
     (create-directory outdir #t)
     (for-each
      (lambda (in)
        (let ((art (input-resolve! in))
              (dir (input-basedir in)))
          (unpack! art (filepath-join root dir))))
      (plan-inputs p))

     (infoln "building")
     (let ((outfile (filepath-join
                     (plan-dir) (plan-hash p)
                     (string-append "build@" (tai64n->string (tai64n-now)) ".log.zst"))))
       ;; if /build fails, put the build log
       ;; into the current directory with a friendly name
       (push-exception-wrapper
        (lambda (exn)
          (let ((linkname (filepath-join
                           (current-directory)
                           (string-append (plan-name p)
                                          "-" (short-hash (plan-hash p))
                                          ".log.zst"))))
            (delete-file* linkname) ;; delete old link if it exists
            (create-symbolic-link outfile linkname)
            (infoln "build failed; please see" linkname)))
        (lambda ()
          (or
           (plan-null-build? p)
           (sandbox-run root outfile)))))

     ;; now save the actual build outputs
     (let ((raw (plan-raw-output p)))
       (if raw
           (file->artifact (filepath-join outdir raw) raw)
           (dir->artifact outdir))))))

;; for-each-anchor traverses a list of plans and artifacts
;; and recursively applies (proc artifact) to each artifact
;; that is a leaf node of the DAG
(define (for-each-anchor proc plst)
  (define ht (make-hash-table hash: string-hash test: string=?))
  (define pl (make-hash-table hash: eq?-hash test: eq?))
  (letrec ((walk (lambda (item)
                   (if (artifact? item)
                       (or (hash-table-ref/default ht (artifact-hash item) #f)
                           (begin
                             (hash-table-set! ht (artifact-hash item) #t)
                             (proc item)))
                       (or (hash-table-ref/default pl item #f)
                           (begin
                             (hash-table-set! pl item #t)
                             (for-each (o walk input-link) (plan-inputs item))))))))
    (for-each walk plst)))

(: build-graph! ((list-of vector) #!rest * -> *))
(define (build-graph! lst #!key (maxprocs (nproc)))
  (let* ((duration       (lambda (from to)
                           (string-append
                            (number->string (exact->inexact (/ (- to from) 1000))) "s")))
         (plan->proc     (make-hash-table test: eq? hash: eq?-hash))
         (hash->plan     (make-hash-table test: string=? hash: string-hash))
         (err            #f)
         (built?         (lambda (plan)
                           (and-let* ((out (plan-outputs plan)))
                                     (file-exists? (artifact-path out)))))
         (done+ok?       (lambda (proc)
                           (let* ((ret (join/value proc))
                                  (st  (proc-status proc)))
                             (and (not (eq? st 'exn)) ret))))
         ;; for a given plan hash, we should only issue one build;
         ;; we use hash->plan to deduplicate equivalent builds
         (sibling?        (lambda (p)
                            (let* ((hash   (plan-hash p))
                                   (winner (hash-table-update!/default hash->plan hash identity p)))
                              (if (eq? p winner) #f winner)))))
    (define (build-proc p)
      (or (hash-table-ref/default plan->proc p #f)
          ;; even though this yields to the child,
          ;; it guarantees that this plan->proc entry
          ;; is visible before other procs are scheduled
          (with-spawn
           build-one!
           (list p)
           (lambda (box)
             (hash-table-set! plan->proc p box)))))
    (define (build-one! p)
      (push-exception-wrapper
       (lambda (exn)
         (plan-exn p exn))
       (lambda ()
         (and
          ;; start all dependencies and ensure they complete ok
          (andmap1 done+ok? (map-unresolved p build-proc))
          (not err)
          (cond
           ;; if we've built this before, we're done
           ((built? p) #t)
           ;; if this plan isn't unique, its exit status
           ;; should be equivalent to that of its identical twin
           ((sibling? p) => done+ok?)
           (else
            (parameterize ((info-prefix (string-append (plan-name p) "-" (short-hash (plan-hash p)) " |")))
              (infoln "queued")
              (let ((qtime (current-milliseconds)))
                (call-with-job
                 (lambda ()
                   (let ((stime (current-milliseconds)))
                     (infoln "starting; queued" (duration qtime stime))
                     (save-plan-outputs! p (plan->outputs! p))
                     (infoln "completed; ran" (duration stime (current-milliseconds))))
                   #t))))))))))
    (with-new-jobserver
     (lambda ()
       (jobserver+ maxprocs)
       ;; for each (unbuilt) explicit package, spawn
       ;; a build coroutine, and then wait for all of them
       (for-each
        join/value
        (foldl1
         (lambda (in lst)
           (if (and (plan? in) (not (built? in)))
               (cons (build-proc in) lst)
               lst))
         '()
         lst))
       ;; now ensure that every plan has exited,
       ;; and determine if we encountered any errors
       (let loop ((err #f)
                  (lst (hash-table-values plan->proc)))
         (if (null? lst)
             (if err (fatal-plan-failure err) #t)
             (let ((head (car lst)))
               (let* ((ret    (join/value head))
                      (threw? (eq? (proc-status head) 'exn)))
                 (loop
                  (or err (and threw? ret))
                  (cdr lst))))))))))

;; build-plan! unconditionally builds a plan
;; and produces its output artifact
;; (it does not save the output)
(: build-plan! (vector -> artifact))
(define (build-plan! top)
  (unless (plan-resolved? top)
    (error "called build-plan! on unresolved plan"))
  (infoln "building" (plan-name top) "-" (short-hash (plan-hash top)))
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
                      (filepath-join (artifact-dir) hash)
                    read))
         (voutput (let ((fp (filepath-join (plan-dir) hash "outputs.scm")))
                    (if (file-exists? fp)
                        (with-input-from-file fp read)
                        #f)))
         (vec->in (lambda (v)
                    (make-input
                     basedir: (vector-ref v 0)
                     link: (vector
                            (vector-ref v 1) (vector-ref v 2) #f))))
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
