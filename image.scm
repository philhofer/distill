;; squashfs produces a squashfs root filesystem from a set of inputs
;;
;; if chown: ((file mode uid gid) ...) is provided, the given
;; files with appear in the root filesysem with those file
;; permissions; otherwise the files will appear as root:root owned
;; with the same file mode as they appear in the input filesystem(s)
(define (squashfs inputs #!key (chown '()) (compress 'zstd))
  ;; mksquashfs accepts 'pseudo-file' definitions
  ;; that let us modify the mode/uid/gid of files;
  (define (pseudo-file)
    (interned "/src/pseudo" #o644
              (with-output-to-string
                (lambda ()
                  (for-each
                    (lambda (pd)
                      (let ((file (car pd))
                            (mode (cadr pd))
                            (uid  (caddr pd))
                            (gid  (cadddr pd)))
                        (display file)
                        (display " m ")
                        (display (number->string mode 8))
                        (display " ")
                        (display uid)
                        (display " ")
                        (display gid)
                        (newline)))
                    chown)))))
  (lambda (conf)
    (let ((out-img "rootfs.img"))
      (make-package
        raw-output: out-img
        label: "squashfs-image"
        src:   (pseudo-file)
        tools: (list execline-tools squashfs-tools)
        inputs: inputs
        build: (let ((opts `(-all-root
                              -pf "/src/pseudo"
                              -comp ,compress)))
                 `((mksquashfs ,($sysroot conf) ,(conc "/out/" out-img) ,@opts)))))))

(define (initramfs inputs #!key (chown '()) (compress 'zstd))
  (unless (null? chown)
    (error "(initramfs ...) doesn't support changing file permissions"))
  (lambda (conf)
    (make-package
      raw-output: "initramfs.zst"
      src:    '()
      label:  "initramfs"
      tools:  (list execline-tools busybox-core zstd libarchive) ;; need bsdtar
      inputs: inputs
      build:  (let ((compressor (case compress
                                  ;; (see note above about compressor nondeterminism)
                                  ((zstd) '((zstd - -o /out/initramfs.zst)))
                                  (else (error "unrecognized compressor" compress)))))
                `((cd ,($sysroot conf))
                  ;; set mtime to 0, since bsdtar(1)
                  ;; does not have an option to override it
                  (if ((find "." -mindepth 1
                             -exec touch -hcd "@0" "{}" ";")))
                  ;; terribly gross hack courtesy of Arch:
                  ;; in order to ensure that the cpio image doesn't
                  ;; include inode numbers, we feed a tar archive
                  ;; back into bsdtar to create a cpio archive
                  (pipeline ((find "." -mindepth 1 -print0)))
                  (pipeline ((sort -z)))
                  (pipeline ((bsdtar --null -vcnf - -T -)))
                  (pipeline ((bsdtar --uid 0 --gid 0 --null -cf - --format=newc "@-")))
                  ,@compressor)))))

;; ext2fs creates a package-lambda that
;; takes everything in 'inputs' and produces
;; an ext2 filesystem image (as a sparse file)
(define (ext2fs name uuid size . inputs)
  (lambda (conf)
    (let* ((outfile '/fs.img)
           (dst     (filepath-join '/out outfile)))
      (make-package
        raw-output: outfile
        label:  name
        tools:  (list busybox-core execline-tools e2fsprogs)
        inputs: inputs
        build:  `((if ((truncate -s ,size ,dst)))
                  ;; can't set this to zero, because mke2fs
                  ;; uses expressions like
                  ;;   x = fs->now ? fs->now : time(NULL);
                  (export E2FSPROGS_FAKE_TIME 1585499935)
                  (export MKE2FS_DETERMINISTIC 1)
                  (mkfs.ext2 -d ,($sysroot conf)
                             -U ,uuid
                             ;; for determinism, use the
                             ;; uuid as the hash seed as well
                             -E ,(string-append
                                   "hash_seed=" uuid)
                             -F -b 4096
                             ,dst))))))

