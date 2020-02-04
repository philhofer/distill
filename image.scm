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
                              -processors $nproc
                              -pf "/src/pseudo"
                              -comp ,compress)))
                 (make-recipe
                   script: (execline*
                             (importas -u "-i" nproc nproc)
                             (mksquashfs ,(sysroot conf) ,(conc "/out/" out-img) ,@opts))))))))

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
                (make-recipe
                  script: (execline*
                            (cd ,(sysroot conf))
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
                            ,@compressor))))))

(define squashfs-tools
  ;; NOTE: mksquashfs before 4.4 does not honor SOURCE_DATE_EPOCH
  ;; nor does it produce reproducible images (without a lot of additional trouble)
  (let* ((version '4.4)
         (src     (remote-archive
                    (conc "https://github.com/plougher/squashfs-tools/archive/" version ".tar.gz")
                    "o-ja9XdUjDj8KcrNOfKi7jQ1z37f7dtf3YUFgqRTIuo="))
         (patch0   (remote-file
                     "https://git.alpinelinux.org/aports/plain/main/squashfs-tools/fix-compat.patch"
                     "JnWyi6aoL93ZiPQNOEelZ1BMkuwo7JCQe1PXddsrO_Y="
                     "/src/patch0.patch"
                     #o644)))
    (lambda (conf)
      (make-package
        label:  (conc "squashfs-tools-" (conf 'arch))
        src:    src
        tools:  (cc-for-target conf)
        inputs: (list zstd lz4 xz-utils zlib musl libssp-nonshared)
        build:  (let ((args (map
                              pair->string=
                              (append '((XZ_SUPPORT . 1)
                                        (LZO_SUPPORT . 0)
                                        (LZ4_SUPPORT . 1)
                                        (ZSTD_SUPPORT . 1)
                                        (XATTR_SUPPORT . 0))
                                      (make-env conf)))))
                  (make-recipe
                    script: (execline*
                              (cd ,(conc "squashfs-tools-" version "/squashfs-tools"))
                              (importas -u "-i" nproc nproc)
                              ;; can't set CFLAGS= in the make invocation
                              ;; because the Makefile is clever and toggles
                              ;; a bunch of additional -DXXX flags based on configuration
                              ,@(export* (cc-env conf))
                              (if ((make -j $nproc ,@args)))
                              (if ((mkdir -p /out/usr/bin)))
                              (cp mksquashfs unsquashfs /out/usr/bin))))))))

(define lz4
  (let* ((version '1.9.2)
         (src     (remote-archive
                    (conc "https://github.com/lz4/lz4/archive/v" version ".tar.gz")
                    "uwHhgT74Tk7ds0TQeFZTodoI1_5IZsRnVRNNHi7ywlc=")))
    (lambda (conf)
      (make-package
        label:  (conc "lz4-" (conf 'arch))
        src:    src
        tools:  (cc-for-target conf)
        inputs: (list musl libssp-nonshared)
        build:  (let ((makeflags (map
                                   pair->string=
                                   (append (cc-env conf) (make-env conf)))))
                  (make-recipe
                    script: (execline*
                              (cd ,(conc "lz4-" version))
                              (importas -u "-i" nproc nproc)
                              (if ((make -j $nproc DESTDIR=/out PREFIX=/usr ,@makeflags install)))
                              ,@(strip-binaries-script conf))))))))

(define zstd
  (let* ((version '1.4.4)
         (src     (remote-archive
                    (conc "https://github.com/facebook/zstd/archive/v"
                          version
                          ".tar.gz")
                    "PKNr93GxvtI1hA4Oia-Ut7HNNGjAcxlvfSr3TYdpdX4=")))
    (lambda (conf)
      (make-package
        label:  (conc "zstd-" (conf 'arch))
        src:    src
        tools:  (cc-for-target conf)
        inputs: (list musl libssp-nonshared)
        build:
        ;; just a raw Makefile
        (let* ((cenv  (cc-env conf))
               (menv  (make-env conf))
               (extra '((HAVE_PTHREAD . 1)
                        (HAVE_ZLIB . 0)
                        (HAVE_LZMA . 0)
                        (HAVE_LZ4 . 0)
                        (ZSTD_LEGACY_SUPPORT . 0)
                        (ZSTD_LIB_DEPRECATED . 0)))
               (makeflags (map pair->string=
                               (append cenv menv extra))))
          (make-recipe
            script: (execline*
                      (cd ,(conc "zstd-" version))
                      (importas -u "-i" nproc nproc)
                      (if ((cd lib/)
                           (make -j $nproc PREFIX=/usr
                                 DESTDIR=/out ,@makeflags install-static install-includes)))
                      (if ((cd programs/)
                           (make -j $nproc ,@makeflags zstd)))
                      (install -D -m "755"
                               programs/zstd /out/usr/bin/zstd))))))))

(define libarchive
  (let* ((version '3.4.1)
         (src     (remote-archive
                    (conc "https://github.com/libarchive/libarchive/releases/download/v" version "/libarchive-" version ".tar.gz")
                    "dfot337ydQKCCABhpdrALARa6QFrjqzYxFSAPSiflFk=")))
    (lambda (conf)
      (make-package
        label:  (conc "libarchive-" (conf 'arch))
        src:    src
        tools:  (cc-for-target conf)
        inputs: (list bzip2 zlib xz-utils lz4 libressl zstd musl libssp-nonshared)
        build:  (gnu-build
                  (conc "libarchive-" version)
                  (config-prepend conf 'configure-flags
                                  '(--without-xml2 --without-acl --without-attr --without-expat)))))))

(define libressl
  (let* ((version '3.0.2)
         (leaf    (remote-archive
                    (conc "https://ftp.openbsd.org/pub/OpenBSD/LibreSSL/libressl-" version ".tar.gz")
                    "klypcg5zlwvSTOzTQZ7M-tBZgcb3tPe72gtWn6iMTR8=")))
    (lambda (conf)
      (make-package
        label:  (conc "libressl-" version "-" (conf 'arch))
        src:    leaf
        tools:  (cc-for-target conf)
        inputs: (list musl libssp-nonshared)
        build:  (gnu-build
                  (conc "libressl-" version)
                  conf
                  post-install: (execline*
                                  (if ((ln -s openssl /out/usr/bin/libressl)))))))))

(define xz-utils
  (let* ((version '5.2.4)
         (src     (remote-archive
                    (conc "https://tukaani.org/xz/xz-" version ".tar.xz")
                    "xbmRDrGbBvg_6BxpAPsQGBrFFAgpb0FrU3Yu1zOf4k8=")))
    (lambda (conf)
      (make-package
        label:  (conc "xz-utils-" (conf 'arch))
        src:    src
        tools:  (cc-for-target conf)
        inputs: (list musl libssp-nonshared)
        build:  (gnu-build
                  (conc "xz-" version) conf)))))
