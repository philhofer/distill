(import
  scheme
  (distill plan)
  (distill package)
  (distill filepath)
  (distill buildenv)
  (distill base)
  (distill image)
  (distill linux)
  (distill service)
  (chicken file)
  (chicken process-context)
  (only (srfi 1) filter)
  (only (srfi 13) substring/shared))

;; this script builds every package in pkg/*.scm
;; (if it is out-of-date)

(define (package-handle name)
  (eval `(import (pkg ,name)))
  (eval name))

(define (trim-suffix str suff)
  (let* ((strlen (string-length str))
         (suflen (string-length suff))
         (diff   (- strlen suflen)))
    (and (>= diff 0)
         (let ((end (substring/shared str diff strlen)))
           (and (string=? end suff)
                (substring/shared str 0 diff))))))

(define package-names
  (find-files
    "./pkg"
    action: (lambda (f lst)
              (let ((suf (trim-suffix f ".scm")))
                (if suf (cons (string->symbol (basename suf)) lst) lst)))
    seed: '()))

;; filter-for-config filters out packages
;; that are known not to cross-compile
(define (filter-for-config conf lst)
  (if (eq? (conf 'arch) *this-machine*)
    lst
    (let ((no-cross '(perl)))
      (filter
        (lambda (p)
          (not (memq p no-cross)))
        lst))))

(define builtin
  `((musl . ,musl)
    (libssp-nonshared . ,libssp-nonshared)
    (libgmp . ,libgmp)
    (libmpfr . ,libmpfr)
    (libmpc . ,libmpc)
    (libisl . ,libisl)
    (zlib   . ,zlib)
    (bzip2  . ,bzip2)
    (gawk   . ,gawk)
    (byacc  . ,byacc)
    (reflex . ,reflex)
    (make   . ,make)
    (skalibs . ,skalibs)
    (busybox-core . ,busybox-core)
    (busybox-full . ,busybox-full)
    (execline-tools . ,execline-tools)
    (xz-utils . ,xz-utils)
    (lz4 . ,lz4)
    (zstd . ,zstd)
    (squashfs-tools . ,squashfs-tools)
    (libarchive . ,libarchive)
    (libressl . ,libressl)
    (s6 . ,s6)
    (s6-rc . ,s6-rc)
    (linux-headers . ,linux-headers)))

(let* ((config  (let ((args (command-line-arguments)))
                  (if (null? args)
                    (build-config)
                    (default-config (string->symbol (car args))))))
       (build!  (config->builder config))
       (names   (filter-for-config config package-names))
       (handles (append (map cdr builtin) (map package-handle names)))
       (outputs (apply build! handles)))
  (for-each
    (lambda (p)
      (display (car p))
      (display " ")
      (display (short-hash (artifact-hash (cdr p))))
      (newline))
    (map cons (append (map car builtin) names) outputs)))
