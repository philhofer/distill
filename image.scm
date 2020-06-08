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
	      (map-lines
	       (lambda (lst)
		 (unless (= (length lst) 4)
		   (error "unexpected \"chown:\" argument" lst))
		 (cons*
		  (car lst) "m" (number->string (cadr lst) 8) (cddr lst)))
	       chown)))
  (lambda (conf)
    (let ((out-img "rootfs.img"))
      (expand-package
       conf
       raw-output: out-img
       label: "squashfs-image"
       src:   (pseudo-file)
       dir:   "/"
       tools: (list execline-tools squashfs-tools)
       inputs: inputs
       build: `(mksquashfs ,$sysroot ,(conc "/out/" out-img)
			   -all-root -pf "/src/pseudo" -comp ,compress)))))

(define (initramfs inputs #!key (chown '()) (compress 'zstd))
  (unless (null? chown)
    (error "(initramfs ...) doesn't support changing file permissions"))
  (lambda (conf)
    (expand-package
     conf
     raw-output: "initramfs.zst"
     src:    '()
     dir:    ($sysroot conf)
     label:  "initramfs"
     tools:  (list execline-tools busybox-core zstd bsdtar)
     inputs: inputs
     build:  (let ((compressor (case compress
				 ((zstd) '(zstd - -o /out/initramfs.zst))
				 (else (error "unrecognized compressor" compress)))))
	       `(;; set mtime to 0, since bsdtar(1)
		 ;; does not have an option to override it
		 if (find "." -mindepth 1
			  -exec touch -hcd "@0" "{}" ";")
		    ;; terribly gross hack courtesy of Arch:
		    ;; in order to ensure that the cpio image doesn't
		    ;; include inode numbers, we feed a tar archive
		    ;; back into bsdtar to create a cpio archive
		    pipeline (find "." -mindepth 1 -print0)
		    pipeline (sort -z)
		    pipeline (bsdtar --null -vcnf - -T -)
		    pipeline (bsdtar --uid 0 --gid 0 --null -cf - --format=newc "@-")
		    ,@compressor)))))

;; ext2fs creates a package-lambda that
;; takes everything in 'inputs' and produces
;; an ext2 filesystem image (as a sparse file)
(define (ext2fs name uuid . inputs)
  (lambda (conf)
    (let* ((outfile '/fs.img)
           (dst     (filepath-join '/out outfile)))
      (expand-package
       conf
       dir: "/"
       raw-output: outfile
       label:  name
       tools:  (list busybox-core execline-tools e2fsprogs)
       inputs: inputs
       build:  `(backtick
		 -n fssize (pipeline (du -sm ,$sysroot)
				     awk "{print $1}")
		 multisubstitute (importas -u |-i| fssize fssize
					   define extra "1")
		 backtick -n size (heredoc 0 "${fssize} + ${extra}"
					   bc)
		 importas -u |-i| size size
		 if (echo "guessing filesystem size is ${size}M")
		 if (truncate -s "${size}M" ,dst)
		 ;; can't set this to zero, because mke2fs
		 ;; uses expressions like
		 ;;   x = fs->now ? fs->now : time(NULL);
		 export E2FSPROGS_FAKE_TIME 1585499935
		 export MKE2FS_DETERMINISTIC 1
		 mkfs.ext2 -d ,$sysroot
		 -U ,uuid
		 ;; for determinism, use the
		 ;; uuid as the hash seed as well
		 -E ,(string-append
		      "hash_seed=" uuid)
		 -F -b 4096
		 ,dst)))))

;; linux-esp creates an ESP ("EFI system partition")
;; that should boot into the given kernel (package) with
;; the given boot arguments
;;
;; (presently, implemented with startup.nsh, which
;; UEFI2.0 says *must* be interpreted on start-up)
(define (linux-esp kernel bootargs)
  (lambda (conf)
    ;; the efi shell will append ".efi"
    ;; to the name of the executable
    (let* ((kname  "vmlinuz")
	   (script (interned
		    "/src/startup.nsh" #o644
		    (lines (list (cons kname bootargs)))))
	   (kfile  (filepath-join ($sysroot conf) "/boot/vmlinuz")))
      (expand-package
       conf
       label: "linux-esp"
       src: (list script)
       dir: "/"
       raw-output: "/esp.img"
       tools:  (list imgtools mtools dosfstools busybox-core execline-tools)
       inputs: (list kernel)
       build:  `(backtick
		 -n size (alignsize -a20 -e1000000 ,kfile /src/startup.nsh)
		 importas -u |-i| size size
		 if (truncate -s $size /out/esp.img)
		 if (mkfs.fat |-i| 77777777 -n "ESP" -F 32 /out/esp.img)
		 if (mcopy -b |-i| /out/esp.img ,kfile ,(string-append "::" kname ".efi"))
		 mcopy -b |-i| /out/esp.img /src/startup.nsh "::startup.nsh")))))

(define (image-rename pkg out perm)
  (let* ((vector-copy (lambda (vec)
			(let ((new (make-vector (vector-length vec))))
			  (vector-copy! vec new)
			  new)))
	 (move-art (lambda (art)
		     (let ((new (vector-copy art)))
		       (vector-set! new 0 `#(file ,out ,perm))
		       new))))
    (lambda (conf)
      (make-input
       basedir: "/"
       link: (if (procedure? pkg)
		 (configure pkg conf)
		 pkg)
       wrap: move-art))))

;; mbr-image produces a (legacy-)bootable image
(define (mbr-image name)
  (lambda (plat rootpkgs)
    (lambda (conf)
      (let ((kern  (platform-kernel plat))
	    (cmdl  (join-with " " (platform-cmdline plat)))
	    (root  (squashfs rootpkgs))
	    (kfile (filepath-join ($sysroot conf) "/boot/vmlinuz"))
	    (rfile (filepath-join ($sysroot conf) "rootfs.img")))
	(expand-package
	 conf
	 label: (string-append name "-mbr-image")
	 raw-output: "/img"
	 tools:  (list mlb2 imgtools execline-tools busybox-core)
	 inputs: (list kern root)
	 build:  `(gptimage
		   -d /out/img (,kfile L ,rfile L)
		   mlb2install /out/img 2048 ,cmdl))))))

;; esp-image produces an EFI-bootable image
(define (efi-image name #!key (uuid #f))
  (lambda (plat rootpkgs)
    (lambda (conf)
      (let ((esp   (linux-esp (platform-kernel plat) (platform-cmdline plat)))
	    (root  (squashfs rootpkgs))
	    (efile (filepath-join ($sysroot conf) "esp.img"))
	    (rfile (filepath-join ($sysroot conf) "rootfs.img")))
	(expand-package
	 conf
	 label:  (string-append name "-efi-image")
	 raw-output: "/img"
	 tools:  (list imgtools execline-tools busybox-core)
	 inputs: (list esp root)
	 build:  `(gptimage
		   -d ,@(if uuid '(-u ,uuid) '())
		   /out/img (,efile U ,rfile L)
		   true))))))
