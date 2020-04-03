(import
  scheme
  (chicken module)
  (only (chicken string) conc)
  (only (chicken file posix) set-file-permissions!)
  (distill hash)
  (distill base)
  (distill plan)
  (distill image)
  (distill package)
  (distill execline)
  (distill filepath)
  (pkg linux-virt-x86_64))

(export qemu-x86_64-kvm)

;; take a kernel package and yield the raw kernel image
(define (raw-vmlinuz kernpkg)
  (lambda (conf)
    (make-package
      raw-output: "/boot/vmlinuz"
      label:  (conc "raw-vmlinuz-" ($arch conf))
      inputs: (list kernpkg)
      tools:  (list execline-tools busybox-core)
      build:  (make-recipe
                script: `((cp -r ,(filepath-join ($sysroot conf) "/boot") /out))))))

(define (qemu-system arch kernel
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
      (set-file-permissions! file #o755))))

;; qemu-x86_64-kvm creates a script for running
;; a x86_64 KVM guest on an x86_64 host
(define (qemu-x86_64-kvm . opts)
  (apply qemu-system 'x86_64 (raw-vmlinuz linux-virt-x86_64) use-kvm: #t opts))
