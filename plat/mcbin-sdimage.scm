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
  (distill fs)

  (pkg jq))

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
      '((if ((test -b /dev/mmcblk1p2)))
	(if -t -n ((test -b /dev/mmcblk1p3)))
	(foreground ((echo "re-partitioning /dev/mmcblk1...")))
	;; sad story here: sfdisk --append doesn't automatically
	;; use the end of the disk, so we have to do some gross hackery
	;; in order to actually determine where mmcblk1p3 should live
	(backtick -n sector ((pipeline ((sfdisk -J /dev/mmcblk1)))
			     (jq -r ".partitiontable.partitions | .[length-1] | .start+.size")))
	(importas -u |-i| sector sector)
	(if ((heredoc 0 "${sector} - L -\n")
	     (sfdisk -f --no-reread --no-tell-kernel --append /dev/mmcblk1)))
	(if ((sync)))
	(hard reboot))))))

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

;; fix an uninitialized data warning in mv_ddr4_training_leveling.c
;; and do NOT generate mv_ddr_build_message.c (it embeds a timestamp)
(define mv-ddr-fixup #<<EOF
--- a/drivers/marvell/mv_ddr/Makefile
+++ b/drivers/marvell/mv_ddr/Makefile
@@ -105,9 +105,6 @@ endif
 # set mv_ddr build message and version string source file
 MV_DDR_VER_CSRC = mv_ddr_build_message.c

-# create mv_ddr build message and version string source file
-$(shell $(MV_DDR_ROOT)/scripts/localversion.sh $(MV_DDR_ROOT) $(MV_DDR_VER_CSRC) 2> /dev/null)
-
 # ******************
 # U-BOOT SPL SUPPORT
 # ******************
--- a/drivers/marvell/mv_ddr/mv_ddr4_training_leveling.c
+++ b/drivers/marvell/mv_ddr/mv_ddr4_training_leveling.c
@@ -368,7 +368,7 @@ static int mv_ddr4_dynamic_pb_wl_supp(u32 dev_num, enum mv_wl_supp_mode ecc_mode
 	u32 subphy_num = ddr3_tip_dev_attr_get(dev_num, MV_ATTR_OCTET_PER_INTERFACE);
 	u8 compare_result = 0;
 	u32 orig_phase;
-	u32 rd_data, wr_data;
+	u32 rd_data, wr_data = 0;
 	u32 flag, step;
 	struct mv_ddr_topology_map *tm = mv_ddr_topology_map_get();
 	u32 ecc_phy_access_id;

EOF
)

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
         (patches    (patch* mv-ddr-fixup))
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
     patches: patches
     extra-src: (list mv-ddr-src
		      mv-ddr-lnk
		      mv-ddr-localversion
		      marvell-scp-bl2-blob)
     env:       '((MAKEFLAGS . "")) ;; do not inherit jobserver; parallel build is broken
     tools:     (list libressl)
     libs:      (list uboot-mcbin)
     no-libc:   #t ; freestanding target
     build:   (let ((mflags `(V=1 (CROSS_COMPILE= ,$cross-compile)
				  SCP_BL2=/src/mrvl_scp_bl2.img
				  PLAT=a80x0_mcbin
				  (BL33= ,$sysroot /boot/u-boot.bin)))
		    (bflags `(V=1 (CROSS_COMPILE= ,$cross-compile)
				  (HOSTCC= ,$build-CC)
				  (HOSTLD= ,$build-LD)
				  (HOSTCCFLAGS= ,$build-CFLAGS))))
		   (cmd*
		     '(cp /src/mv_ddr_build_message.c drivers/marvell/mv_ddr/)
		     ;; do not force a clean on every make invocation:
		     '(sed "-i" -e "/^mrvl_clean:/,+3d" plat/marvell/marvell.mk)
		     '(sed "-i" -e "s/mrvl_clean//g" plat/marvell/marvell.mk)
		     ;; busybox truncate(1) doesn't support '%size' format
		     '(sed "-i" -e "s/ %128K / 128K /" plat/marvell/a8k/common/a8k_common.mk)
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
  (let ((kernel (linux/config-static "mcbin" "7T9BGGKMOBpAtgHatOv8gRA2Nf92jDWptxrJLV9T3ms="
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
     packages: (list mcbin-preboot sfdisk jq)
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
			build:  `((backtick -n fwsize ((alignsize -a20 -e2097152 ,fw)))
				  (importas -u |-i| fwsize fwsize)
				  ;; mmcblk1p1 is the boot partition;
				  ;; mmcblk1p2 is the root partition;
				  ;; the firmware lives at mmcblk1 +2M
				  (gptimage -d -b $fwsize /out/img ((,p1 L) (,p2 L)))
				  ;; REMOVE ME:
				  (dd ,(string-append "if=" fw) of=/out/img bs=1M seek=2 conv=notrunc))))))))))
