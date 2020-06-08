(import
  scheme
  (distill plan)
  (distill eprint)
  (distill package)
  (distill filepath)
  (distill base)
  (chicken file)
  (chicken process-context)

  (pkg chicken))

(define (filter pred? lst)
  (if (null? lst)
    '()
    (let* ((head (car lst))
           (rest (cdr lst))
           (tail (filter pred? rest)))
      (if (pred? head)
        (cons head tail)
        tail))))

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
         (let ((end (##sys#substring str diff strlen)))
           (and (string=? end suff)
                (##sys#substring str 0 diff))))))

;; alist of arch-limited packages
;; (i.e. packages that only support
;; a subset of host architectures)
(define pkgs-for-arch
  '((linux-virt-x86_64 x86_64)))

(define package-names
  (find-files
   "./pkg"
   action: (lambda (f lst)
	     (let ((suf (trim-suffix f ".scm")))
	       (if suf
		   (cons (string->symbol (basename suf)) lst)
		   lst)))
   seed: '()))

;; filter out packges that are arch-specific
(define (packages-for conf)
  (let ((arch ($arch conf)))
    (filter
     (lambda (name)
       (let ((c (assq name pkgs-for-arch)))
	 (or (not c) (memq arch (cdr c)))))
     package-names)))

(define (builtin-for conf)
  (letrec-syntax ((builtin* (syntax-rules ()
                              ((_) '())
                              ((_ (name expr) rest* ...)
                               (let ((tail (builtin* rest* ...)))
                                 (if expr
                                   (cons (cons (quote name) name) tail)
                                   tail)))
                              ((_ name rest* ...)
                               (cons (cons (quote name) name) (builtin* rest* ...))))))
    (builtin*
      musl
      libssp-nonshared
      libgmp
      libmpfr
      libmpc
      libisl
      zlib
      bzip2
      gawk
      byacc
      reflex
      make
      skalibs
      busybox-core
      busybox-full
      execline-tools
      xz-utils
      lz4
      zstd
      squashfs-tools
      libarchive
      libressl
      s6
      s6-rc
      linux-headers
      libelf
      exportall
      e2fsprogs
      dosfstools
      mtools
      imgtools
      nasm
      mlb2
      hard
      libmnl
      libnftnl
      iptables
      iproute2
      matchable-egg
      srfi-13-egg
      srfi-14-egg
      srfi-69-egg      
      (perl (eq? ($arch conf) *this-machine*)))))

(let* ((config  (let ((args (command-line-arguments)))
                  (if (null? args)
                    (force default-build-config)
                    (default-config (string->symbol (car args))))))
       (build!  (config->builder config))
       (loaded  (map (lambda (name)
                       (cons name (package-handle name)))
                     (packages-for config)))
       (builtin (builtin-for config))
       (all     (append loaded builtin))
       (outputs (apply build! (map cdr all))))
  (for-each
    (lambda (name out)
      (display name)
      (display " ")
      (display (short-hash (artifact-hash out)))
      (newline))
    (map car all)
    outputs))
(exit 0)
