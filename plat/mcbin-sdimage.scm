(import
  scheme
  (only (chicken base) unless error)
  (only (chicken string) conc)
  (distill kvector)
  (distill filepath)
  (distill execline)
  (distill base)
  (distill plan)
  (distill service)
  (distill sysctl)
  (distill net)
  (distill package)
  (distill system)
  (distill image)
  (distill fs))

;; root disk info:
;;  - we're booting from the SD card slot, which is mmcblk1
;;  - ATF is loaded from mmcblk1 +2M by the boot ROM
;;  - ATF chainloads u-boot, which we configure to load
;;    the kernel and fdt from mmcblk1p1 (formatted as ext2)
;;  - the kernel is booted with root=mmcblk1p2, and it will
;;    create mmcblk1p3 with remaining space on the SD card
;;    if it doesn't exist


;; see include/configs/mvebu_armada-8k.h
;; in the u-boot source tree; these are
;; what upstream thinks are suitable addresses
;; for loading blobs before booti, bootm, etc.
;;
;; the sd card slot is mmcblk1; on-board eMMC is mmcblk0
(define uboot-env
  (k=v*
    scriptaddr:     "0x4d00000"
    pxefile_addr_r: "0x4e00000"
    fdt_addr_r:     "0x4f00000"
    kernel_addr_r:  "0x5000000"
    ramdisk_addr_r: "0x8000000"
    fdtfile:        "armada-8040-mcbin.dtb"
    loadkernel:     "ext2load mmc 1:1 ${kernel_addr_r} boot/vmlinuz"
    loadfdt:        "ext2load mmc 1:1 ${fdt_addr_r} boot/${fdtfile}"
    bootargs:       "root=/dev/mmcblk1p2 rootfstype=squashfs"
    bootcmd:        "run loadkernel loadfdt; booti ${kernel_addr_r} - ${fdt_addr_r}"))

(define uboot-mcbin
  (uboot/config
    "mcbin"
    "7OANoghNx5M20pf41I4ByZ7rYc_umDft5VJkcYjyUk0="
    uboot-env
    "run bootcmd"))

(define mcbin-preboot
  (interned
   "/sbin/preboot" #o744
   (lambda ()
     (write-exexpr
      '(if (test -b /dev/mmcblk1p2)
	   if -t -n (test -b /dev/mmcblk1p3)
	   foreground (echo "re-partitioning /dev/mmcblk1...")
	   if (dosextend -n3 -k /dev/mmcblk1)
	   test -b /dev/mmcblk1p3)))))

;; TODO: reverse-engineer this.
;; FreeRTOS is somewhere in here.
;; This parser for these images in ATF
;; is at plat/marvell/common/mss/mss_scp_bootloader.c,
;; and they *look* like thumb(2?) binaries for a Cortex-M3
;; (there are apparently some coprocessors running on-chip,
;; but the documentation appears to be gated by an NDA)
(define marvell-scp-bl2-blob
  (remote-file
    "https://github.com/MarvellEmbeddedProcessors/binaries-marvell/raw/binaries-marvell-armada-18.12/mrvl_scp_bl2.img"
    "AN6vF_jBemdvGpaOYjDvnxfo8TAkxZhS1eKF0viY8l8="
    "/src/mrvl_scp_bl2.img"
    #o644))

;; the default mv_ddr makefile embeds a build timestamp;
;; simply replace the file that localversion.sh generates
;; with this:
(define mv-ddr-localversion
  (interned
    "/src/mv_ddr_build_message.c"
    #o644
    (string-append
      "const char *mv_ddr_build_message = \"(distill - reproducible)\";\n"
      "const char *mv_ddr_version_string = \"mv_ddr-devel-18.08.0-distill\";\n")))

(define atf-mcbin
  ;; it took some serious yak-shaving to get this to build
  ;; correctly and reproducibly... here's what's going on:
  ;;
  ;; - the a80x0 hardware needs out-of-tree DRAM-training code
  ;;   to be vendored into the drivers/marvell directory
  ;; - the marvell makefiles are super busted: parallel builds
  ;;   fail (due to broken prerequisites that I have not had the
  ;;   patience to track down) and there are strange superfluous
  ;;   rules like a non-PHONY 'clean' rule that runs on every invocation
  ;; - there's a blob with firmware for the a80x0 coprocessors
  ;;   that needs to be part of the build; I have not had time
  ;;   to reverse-engineer them (but they appear to be thumb-mode
  ;;   code for cortex-M3 cores, and the license indicates that
  ;;   the code is derived from FreeRTOS)
  ;;
  (let* ((name "arm-trusted-firmware")
	 (ver  "2.2")
         (mv-ddr-ver 'mv_ddr-armada-atf-mainline)
         (mv-ddr-src (remote-archive
                       ;; TODO: this is not guaranteed to be a stable tarball,
                       ;; but marvell has all but abandoned this code...
                       (conc "https://github.com/MarvellEmbeddedProcessors/mv-ddr-marvell/archive/" mv-ddr-ver ".tar.gz")
                       "nV3OhnbmGNAy4JQz60aYjucT1SrMuoVHppiYbyd40zI="))
	 (dir        (string-append "/" name "-" ver))
	 ;; symlink the mv_ddr source into the atf source tree:
	 (mv-ddr-lnk (interned-symlink
		      (filepath-join dir "drivers/marvell/mv_ddr")
		      (conc "/mv-ddr-marvell-" mv-ddr-ver))))
    (cc-package
     name ver
     "https://github.com/ARM-software/$name/archive/v$version.tar.gz"
     "Pxwp8bIs5lmYYmedB8SjKH-3bLNV5_1b0TKdAZCGWm4="
     raw-output: "/boot/flash-image.bin"
     use-native-cc: #t
     extra-src: (list mv-ddr-src
		      mv-ddr-lnk
		      mv-ddr-localversion
		      marvell-scp-bl2-blob)
     env:       '((MAKEFLAGS . "")) ;; do not inherit jobserver; parallel build is broken
     tools:     (list libressl)
     libs:      (list uboot-mcbin)
     no-libc:   #t
     build:   (let ((mflags `(V=1 ,(elconc 'CROSS_COMPILE= $cross-compile)
				  SCP_BL2=/src/mrvl_scp_bl2.img
				  PLAT=a80x0_mcbin
				  ,(elconc 'BL33= $sysroot '/boot/u-boot.bin)))
		    (bflags `(V=1 ,(elconc 'CROSS_COMPILE= $cross-compile)
				  ,(el= 'HOSTCC= $build-CC)
				  ,(el= 'HOSTLD= $build-LD)
				  ,(el= 'HOSTCCFLAGS= $build-CFLAGS))))
		   (elif*
		     '(cp /src/mv_ddr_build_message.c drivers/marvell/mv_ddr/)
		     ;; do not force a clean on every make invocation:
		     '(sed "-i" -e "/^mrvl_clean:/,+3d" plat/marvell/marvell.mk)
		     '(sed "-i" -e "s/mrvl_clean//g" plat/marvell/marvell.mk)
		     ;; busybox truncate(1) doesn't support '%size' format
		     '(sed "-i" -e "s/ %128K / 128K /" plat/marvell/a8k/common/a8k_common.mk)
		     ;; quiet unused variable warning from gcc
		     '(sed "-i" -e "s/u32 rd_data, wr_data;/u32 rd_data, wr_data=0;/g" drivers/marvell/mv_ddr/mv_ddr4_training_leveling.c)
		     ;; remove rule for mv_ddr_build_message.c; we create our own
		     '(sed "-i" -e "/^# create mv_ddr build/,+1d" drivers/marvell/mv_ddr/Makefile)
		     ;; the top-level makefile doesn't invoke these sub-makes
		     ;; correctly, so invoke them ahead of time with
		     ;; the right overrides...
		     `(make ,@bflags "LDLIBS=-static-pie -lcrypto"
			    -C tools/fiptool all)
		     `(make ,@bflags "DOIMAGE_LD_FLAGS=-static-pie"
			    -C tools/marvell/doimage all)
		     `(make ,@mflags "SCP_BL2=/src/mrvl_scp_bl2.img" all fip)
		     '(install -D -m "644" -t /out/boot build/a80x0_mcbin/release/flash-image.bin))))))

(define mcbin-sdimage
  (let ((kernel (linux/config-static "mcbin" "patches/linux/config.mcbin.aarch64"
				     dtb: 'arch/arm64/boot/dts/marvell/armada-8040-mcbin.dtb)))
    (make-platform
     config:   (gcc+musl-static-config
		'aarch64
		optflag: '-O2
		sspflag: '-fstack-protector-strong
		;; compiling with -mcpu=... helps us get some
		;; additional coverage on our GCC recipe with
		;; cross-compilation, because this is an illegal
		;; flag for any compiler/architecture except aarch64,
		;; so builds will fail if this flag leaks into the
		;; wrong CFLAGS arguments
		extra-cflags: '(-mcpu=cortex-a72)
		build: (force default-build-config))
     kernel:   kernel
     cmdline:  '("root=/dev/mmcblk1p2 rootfstype=squashfs") ; see uboot cmdline
     packages: (list mcbin-preboot imgtools)
     services: (list (var-mount "/dev/mmcblk1p3"))
     mkimage:  (lambda (plat rootpkgs)
		 (let* ((kern (platform-kernel plat))
			(root (squashfs rootpkgs))
			(boot (ext2fs "mbcin/boot" "14bcce9f-5096-4fa8-bdb6-51b0b12823f1" kern)))
		   (lambda (conf)
		     (let ((fw (filepath-join ($sysroot conf) "/boot/flash-image.bin"))
			   (p1 (filepath-join ($sysroot conf) "/fs.img"))
			   (p2 (filepath-join ($sysroot conf) "/rootfs.img")))
		       (expand-package
			conf
			label: "mcbin-sdimage"
			raw-output: "/img"
			tools:  (list sfdisk imgtools execline-tools busybox-core)
			inputs: (list boot atf-mcbin root)
			build:  `(backtick -n fwsize (alignsize -a20 -e2097152 ,fw)
					   importas -u |-i| fwsize fwsize
					   ;; mmcblk1p1 is the boot partition;
					   ;; mmcblk1p2 is the root partition;
					   ;; the firmware lives at mmcblk1 +2M
					   gptimage -d -b $fwsize /out/img (,p1 L ,p2 L)
					   ;; REMOVE ME:
					   dd ,(string-append "if=" fw) of=/out/img bs=1M seek=2 conv=notrunc)))))))))
