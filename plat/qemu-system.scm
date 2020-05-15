(import
  scheme
  (chicken module)
  (only (chicken string) conc)
  (only (chicken file posix) set-file-permissions!)
  (distill hash)
  (distill base)
  (distill plan)
  (distill eprint)
  (distill image)
  (distill package)
  (distill execline)
  (distill filepath)
  (pkg linux-virt-x86_64))

(export
 qemu-script
 qemu-system-x86_64-image)

;; vmlinuz wraps a kernel package
;; and produces /boot/vmlinuz as a raw output
(define (vmlinuz kernpkg)
  (lambda (conf)
    (let ((kernplan (configure kernpkg conf)))
      (make-plan
       name:   (string-append "vmlinuz-" (plan-name kernplan))
       inputs: (list (make-input basedir: "/out" link: kernplan))
       null-build: #t
       raw-output: "/boot/vmlinuz"))))

(define (qemu-script arch kernel
                     #!key
                     (use-var #t)     ;; generated scripts accept /var fs image as first argument
                     (use-kvm #t)     ;; add --use-kvm -cpu host
                     (cmdline '())    ;; additional linux cmdline options
                     (qemu-args '())) ;; additional qemu-system-xxx arguments
  (lambda (rootpkgs)
    (let* ((bin  (conc "qemu-system-" arch))
           (conf (default-config arch))
           (bld! (config->builder conf))
           (arts (bld! kernel (squashfs rootpkgs)))
           (kern (car arts))
           (root (cadr arts))
           (hash (hash-of (artifact-hash kern) (artifact-hash root)))
           (file (conc "vm-" arch "-" (substring hash 0 10))))
      (with-output-to-file
        file
        (lambda ()
          (write-exexpr
            `((multisubstitute ((define root ,(artifact-path root))
                                (define kernel ,(artifact-path kern))
                                (define cmdline ,(string-append "root=/dev/vda rootfstype=squashfs console=ttyS0"
                                                                (spaced cmdline)))))
              (,bin ,@(if use-kvm '(-cpu host --enable-kvm) '())
                    ;; ensure that the squashfs root is truly read-only with media=cdrom
                    -drive "file=${root},if=virtio,format=raw,media=cdrom"
                    ,@(if use-var '(-drive "file=${1},if=virtio,format=raw") '())
                    -kernel "${kernel}"
                    -append "${cmdline}"
                    ,@qemu-args
                    -nographic $@))
            shebang: (if use-var
                       "#!/bin/execlineb -s1"
                       "#!/bin/execlineb -s0"))))
      (set-file-permissions! file #o755)
      (info "output script is" file))))

(define qemu-system qemu-script)

;; qemu-system-x86_64-image takes a kernel
;; and yields a platform function for qemu-system-x86_64
;; (the host arch is presumed to be x86_64, obviously)
(define (qemu-system-x86_64-image kernel)
  (lambda (rootpkgs)
    (let* ((img (diskparts
		 `((,(vmlinuz kernel) L)
		   (,(squashfs rootpkgs) L))
		 format: 'dos
		 legacy-boot: "root=/dev/vda2 rootfstype=squashfs console=ttyS0"))
	   (conf (default-config 'x86_64))
	   (bld! (config->builder conf))
	   (img  (car (bld! img))))
      (info "output vm image is" (artifact-path img)))))
