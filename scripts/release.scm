(import
 scheme
 (distill base)
 (distill plan)
 (distill package)
 (distill execline)
 (distill filepath)
 (distill hash)
 (chicken process)
 (chicken process-context)
 (chicken port)
 (chicken file)
 (chicken io) ; read-string
 (chicken string)
 (pkg chicken))

(define (without syms lst)
  (if (null? lst)
      lst
      (let ((head (car lst)))
        (if (memq head syms)
            (cdr lst)
            (cons head (without syms (cdr lst)))))))

(define (sh->string cmdline)
  (with-input-from-pipe cmdline read-string))

(define (current-git-sha)
  (string-chomp (sh->string "git log -1 --format='%H'")))

;; run 'git archive' on HEAD and
;; yield the artifact pointing at
;; the source tarball
(define (git-archive)
  (with-input-from-pipe
   "git archive --format=tar --prefix=distill/ HEAD | zstd -c"
   (lambda ()
     (let* ((dst   (filepath-join (artifact-dir) "release.tmp"))
            (oport (open-output-file dst))
            (hash  (copy-port+hash (current-input-port) oport)))
       (close-output-port oport)
       (rename-file dst (filepath-join (artifact-dir) hash) #t)
       (local-archive 'tar.zst hash)))))

(define (release-package git-sha archive)
  (let (($cflags (lambda (conf)
                   ;; due to chicken's compilation model, essentially every
                   ;; continuation is noreturn, so the stack protector does
                   ;; literally nothing
                   (without '(-fstack-protector -fstack-protector-strong -fstack-protector-all)
                            ($CFLAGS conf)))))
    (cc-package
     "distill" git-sha
     #f
     archive
     dir: "/distill"
     tools: (list chicken)
     libs:  (list libarchive libzstd)
     build: `(make DESTDIR=/out
               PREFIX=/usr
               CSI=/usr/bin/csi
               CHICKEN=/usr/bin/chicken
               ,(el= 'VERSION= git-sha)
               ,(el= 'CHICKEN_FEATURES= $chicken-features)
               ,(el= 'AR= $AR)
               ,(el= 'CC= $CC)
               ,(el= 'CFLAGS= $cflags)
               ,(el= 'LDFLAGS= $LDFLAGS)
               install))))

(let* ((println (lambda args
                  (for-each display args) (newline)))
       (args    (command-line-arguments))
       (arch    (if (null? args) *this-machine* (string->symbol (car args))))
       (conf    (default-config arch))
       (build!  (config->builder conf))
       (sha     (current-git-sha))
       (archive (git-archive))
       (package (release-package sha archive))
       (out     (build! package)))
  (println "git " sha)
  (println "tarball " (artifact-hash archive))
  (println "binary " (artifact-hash (car out)))
  (exit 0))
