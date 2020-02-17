;; swap is a service that uses 'dev' for swap
(define (swapon dev)
  (make-service
    name:   (string->symbol (string-append "swap." dev))
    inputs: '()
    after:  (list dev)
    spec:   (list
              type: 'oneshot
              up:   (execline*
                      ;; TODO: detect swap and don't mkswap
                      (fdmove -c 2 1)
                      (foreground ((mkswap ,dev)))
                      (swapon ,dev))
              down: (execline*
                      (fdmove -c 2 1)
                      (foreground ((swapoff ,dev)))
                      (true)))))

;; logging services will generally depend on var-mounted-rw
(define var-mounted-rw 'var-mount)

;; var-mount creates a read-write mount at /var
;;
;; note that the var-mount service is a requirement
;; for persistent logs when the system has a read-only rootfs
(define (var-mount dev)
  (let ((opts   '(rw nosuid nodev noexec noatime data=ordered))
        (mkopts '()))
    (make-service
      name:   var-mounted-rw
      inputs: (list e2fsprogs)
      after:  (list dev)
      spec:   (list
                type: 'oneshot
                up:   (execline*
                        (fdmove -c 2 1)
                        (if ((test -b ,dev)))
                        ;; TODO: figure out a better way
                        ;; to determine if this device has actually
                        ;; been formatted yet...
                        (if ((if -t -n ((fsck.ext4 -p ,dev)))
                             (foreground ((echo "fsck didn't work; running mkfs.ext4 on /var ...")))
                             (mkfs.ext4 ,@mkopts ,dev)))
                        (mount -t ext4 -o ,(join/s "," (list->seq opts)) ,dev /var))
                down: (execline*
                        (fdmove -c 2 1)
                        (foreground ((umount /var)))
                        (true))))))

(define buildcc-env
  (let ((env   cc-env/build)
        (chomp (string-length "_FOR_BUILD"))
        (pre   "BUILD_"))
    (map
      (lambda (p)
        (let* ((lhs (symbol->string (car p)))
               (rhs (cdr p))
               (sub (substring/shared lhs 0 (- (string-length lhs) chomp))))
          (cons
            (string->symbol (string-append pre sub))
            rhs)))
      env)))

(define e2fsprogs
  (let* ((version '1.45.5)
         (src     (remote-archive
                    (conc "https://kernel.org/pub/linux/kernel/people/tytso/e2fsprogs/v" version "/e2fsprogs-" version ".tar.xz")
                    "w7R6x_QX6QpTEtnNihjwlHLBBtfo-r9RrWVjt9Nc818=")))
    (lambda (conf)
      (make-package
        label:  (conc "e2fsprogs-" version "-" (conf 'arch))
        src:    src
        tools:  (append (cc-for-target conf)
                        (native-toolchain-for conf))
        inputs: (list linux-headers musl libssp-nonshared)
        build:  (gnu-build
                  (conc "e2fsprogs-" version)
                  (config-prepend conf 'configure-flags
                                  `(--enable-symlink-install
                                     --enable-libuuid
                                     --enable-libblkid
                                     --disable-uuidd
                                     --disable-fsck
                                     ,@(map pair->string= buildcc-env)))
                  install-flags: '("MKDIR_P=install -d" DESTDIR=/out install install-libs))))))

