(import
  scheme
  (scheme base)
  (distill base)
  (distill plan)
  (distill package)
  (distill execline)
  (only (chicken string) conc)
  (only (chicken module) export)

  (pkg lz4)
  (pkg zstd)
  (pkg xz-utils))

(export mksquashfs)

(define squashfs-tools
  (let* ((version '4.4)
         (src     (remote-archive
                    (conc "https://github.com/plougher/squashfs-tools/archive/" version ".tar.gz")
                    "o-ja9XdUjDj8KcrNOfKi7jQ1z37f7dtf3YUFgqRTIuo="))
         (patch0   (remote-file
                     "https://git.alpinelinux.org/aports/plain/main/squashfs-tools/fix-compat.patch"
                     "JnWyi6aoL93ZiPQNOEelZ1BMkuwo7JCQe1PXddsrO_Y="
                     "/src/patch0.patch"
                     #x644)))
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

(define (mksquashfs inputs #!key (compress 'zstd))
  (error "mksquashfs: unimplemented"))
