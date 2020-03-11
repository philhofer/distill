;; swap is a service that uses 'dev' for swap
(define (swapon dev)
  (make-service
    name:   (string->symbol (string-append "swap." dev))
    inputs: '()
    after:  (list dev)
    spec:   (oneshot*
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
      spec:   (oneshot*
                up: (execline*
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
                        (foreground ((mount -o "ro,remount,noexec,nosuid" ,dev /var)))
                        (foreground ((sync)))
                        (foreground ((umount /var)))
                        (true))))))

;; e2fsprogs is weird and uses BUILD_CC, BUILD_CFLAGS, etc.
;; in order to indicate which CC to use for building tools
(define buildcc-env
  (cc-env/build
    (lambda (kw)
      (string->keyword
        (string-append
          "BUILD_"
          (keyword->string kw))))))

;; mke2fs -d <dir> does not have a way
;; to bypass timestamps and override uid/gid,
;; so this hacks in a way to at least ensure
;; that those fields get zeroed...
(define mke2fs-repro-patch #<<EOF
--- a/misc/create_inode.c
+++ b/misc/create_inode.c
@@ -17,6 +17,7 @@
 #include <time.h>
 #include <sys/stat.h>
 #include <sys/types.h>
+#include <stdlib.h>
 #include <unistd.h>
 #include <limits.h> /* for PATH_MAX */
 #include <dirent.h> /* for scandir() and alphasort() */
@@ -128,6 +129,16 @@
 	inode.i_atime = st->st_atime;
 	inode.i_mtime = st->st_mtime;
 	inode.i_ctime = st->st_ctime;
+
+        if (getenv("MKE2FS_DETERMINISTIC")) {
+                inode.i_uid = 0;
+                ext2fs_set_i_uid_high(inode, 0);
+                inode.i_gid = 0;
+                ext2fs_set_i_gid_high(inode, 0);
+                inode.i_atime = 0;
+                inode.i_mtime = 0;
+                inode.i_ctime = 0;
+        }

 	retval = ext2fs_write_inode(fs, ino, &inode);
 	if (retval)
EOF
)

(define e2fsprogs
  (let* ((version '1.45.5)
         (src     (remote-archive
                    (conc "https://kernel.org/pub/linux/kernel/people/tytso/e2fsprogs/v" version "/e2fsprogs-" version ".tar.xz")
                    "w7R6x_QX6QpTEtnNihjwlHLBBtfo-r9RrWVjt9Nc818="))
         (patches (patch* mke2fs-repro-patch)))
    (lambda (conf)
      (make-package
        label:  (conc "e2fsprogs-" version "-" ($arch conf))
        src:    (cons src patches)
        tools:  (append (cc-for-target conf)
                        (native-toolchain-for conf))
        inputs: (list linux-headers musl libssp-nonshared)
        build:  (gnu-recipe
                  (conc "e2fsprogs-" version)
                  (kwith
                    ($gnu-build conf)
                    pre-configure: (+= (script-apply-patches patches))
                    configure-args: (+=
                                      `(--enable-symlink-install
                                         --enable-libuuid
                                         --enable-libblkid
                                         --disable-uuidd
                                         --disable-fsck
                                         ,@(kvargs buildcc-env)))
                    install-flags: (:= '("MKDIR_P=install -d" DESTDIR=/out install install-libs))))))))

;; kmsg is a super lightweight syslogd-style service
;; that reads logs from /dev/kmsg and stores them in
;; /var/log/services/kmsg, taking care to compress
;; (and eventually delete) old logs
(define kmsg
  (let ((dir  '/var/log/services/kmsg)
        (opts '(t n10 s1000000 "!zstd -c -"))
        (nfd  3))
    (make-service
      name:   'kmsg
      inputs: (list zstd)
      after:  (list var-mounted-rw)
      spec:   (longrun*
                notification-fd: nfd
                run: `((fdmove -c 2 1)
                       (if ((mkdir -p ,dir)))
                       (if ((chown -R catchlog:catchlog ,dir)))
                       (if ((chmod "2700" ,dir)))
                       ;; strip off timestamps and have s6-log prepend tai64n timestamps instead
                       (pipeline ((redirfd -r -b 0 /dev/kmsg)
                                  (s6-setuidgid catchlog)
                                  (sed -e "s/^[0-9].*-;//g")))
                       (s6-setuidgid catchlog)
                       (s6-log -b -d ,nfd -- ,@opts /var/log/services/kmsg))))))

;; log-services creates a mount at /var
;; using "var-dev" and then maps the list
;; of services "svcs" to a new list of services
;; that include loggers for each longrun service
(define (var-log-services var-dev svcs #!key
                          ;; keep at most 10 old logs
                          ;; and rotate at 1MB, using
                          ;; zstd to compress old logs
                          (log-opts '(t n10 s1000000
                                        "!zstd -c -")))
  (let ((logdir     (lambda (svc)
                      (conc
                        "/var/log/services/"
                        (service-name svc))))
        ;; TODO: better detection of logging options,
        ;; compression, etc.
        (log-inputs (if (member "!zstd -c -" log-opts)
                      (list zstd)
                      '())))
    (foldl
      (lambda (lst old)
        ;; only touch longrun services
        ;; that do not already have a producer-for
        (if (or (not (longrun? (service-spec old)))
                (kref* old producer-for:))
          (cons old lst)
          (let ((ospec  (service-spec old))
                (oafter (service-after old))
                (lname  (conc (service-name old) "-log"))
                (dir    (logdir old)))
            (cons
              (make-service
                name:   lname
                inputs: log-inputs
                after: (list var-mounted-rw)
                spec:  (longrun*
                         consumer-for: (service-name old)
                         dependencies: (conc var-mounted-rw "\n")
                         notification-fd: 3
                         run: `((if ((mkdir -p ,dir)))
                                (if ((chown -R catchlog:catchlog ,dir)))
                                (if ((chmod "2700" ,dir)))
                                (s6-setuidgid catchlog)
                                (exec -c)
                                (s6-log -d 3 ,@log-opts ,dir))))
              (cons
                (update-service
                  old
                  after: (cons var-mounted-rw oafter)
                  spec:  (kupdate ospec producer-for: lname))
                lst)))))
      (list (var-mount var-dev)
            kmsg)
      svcs)))
