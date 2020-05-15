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
       build: (let ((opts `(-all-root
			    -pf "/src/pseudo"
			    -comp ,compress)))
		`((mksquashfs ,($sysroot conf) ,(conc "/out/" out-img) ,@opts)))))))

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
				 ((zstd) '((zstd - -o /out/initramfs.zst)))
				 (else (error "unrecognized compressor" compress)))))
	       `(;; set mtime to 0, since bsdtar(1)
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
      (expand-package
       conf
       dir: "/"
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
       build:  `((backtick -n size ((alignsize -a20 -e1000000 ,kfile /src/startup.nsh)))
		 (importas -u |-i| size size)
		 (if ((truncate -s $size /out/esp.img)))
		 (if ((mkfs.fat |-i| "77777777" -n "ESP" -F 32 /out/esp.img)))
		 (if ((mcopy -b |-i| /out/esp.img ,kfile ,(string-append "::" kname ".efi"))))
		 (mcopy -b |-i| /out/esp.img /src/startup.nsh "::startup.nsh"))))))

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

;; diskparts takes a list of partition specifications
;; and assembles them into a single disk image
(define (diskparts parts #!key
		   (format 'gpt)
		   (uuid #f)
		   (size #f)
		   (legacy-boot #f))
  (let* ((partfile  (lambda (n)
		      (string-append "/images/part" (number->string n))))
	 ;; walk through the part spec and
	 ;; collect everything that is a package or artifact
	 (partfiles (let loop ((i 1)
			       (parts parts)
			       (out '()))
		      (if (null? parts)
			  out
			  (let* ((spec (car parts))
				 (rest (cdr parts))
				 (head (car spec)))
			    (if (or (procedure? head) (artifact? head))
				(loop
				 (+ i 1)
				 rest
				 (cons (image-rename head (partfile i) #o444) out))
				(loop (+ i 1) rest out))))))
	 ;; replace every package or plan in the spec
	 ;; with the name of the assigned image file
	 (partspec (let loop ((i   1)
			      (lst parts))
		     (if (null? lst)
			 '()
			 (let* ((spec (car lst))
				(rest (cdr lst))
				(head (car spec)))
			   (cons
			    (if (or (artifact? head) (procedure? head))
				(cons (partfile i) (cdr spec))
				spec)
			    (loop (+ i 1) rest)))))))
    (lambda (conf)
      (expand-package
       conf
       label: "disk-image"
       raw-output: "/img"
       src:   '()
       env:   '()
       dir:   "/"
       tools: (let ((lst (list imgtools diskutils execline-tools busybox-core)))
		(if legacy-boot (cons mlb2 lst) lst))
       inputs: partfiles
       build:  `((gptimage
		  ,@(case format
		      ((dos) '(-d))
		      ((gpt) '())
		      (else (error "diskparts: invalid format" format)))
		  ,@(if uuid `(-u ,uuid) '())
		  ,@(if size `(-s ,size) '())
		  /out/img ,partspec)
		 ,@(if legacy-boot
		       `((mlb2install /out/img 2048 ,legacy-boot))
		       '((true))))))))
