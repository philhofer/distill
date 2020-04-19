(import
  scheme
  (only (chicken base) unless error)
  (only (chicken string) conc)
  (distill kvector)
  (distill base)
  (distill plan)
  (distill service)
  (distill sysctl)
  (distill net)
  (distill package)
  (distill system)
  (distill image)
  (distill fs))

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
    loadkernel:     "ext2load mmc 1:2 ${kernel_addr_r} boot/vmlinuz"
    loadfdt:        "ext2load mmc 1:2 ${fdt_addr_r} boot/${fdtfile}"
    bootargs:       "root=/dev/mmcblk1p3 rootfstype=squashfs"
    bootcmd:        "run loadkernel loadfdt; booti ${kernel_addr_r} - ${fdt_addr_r}"))

(define uboot-mcbin
  (uboot/config
    "mcbin"
    "7OANoghNx5M20pf41I4ByZ7rYc_umDft5VJkcYjyUk0="
    uboot-env
    '()
    "run bootcmd"))

;; TODO: reverse-engineer this.
;; FreeRTOS is somewhere in here.
;; This parser for these images in ATF
;; is at plat/marvell/common/mss/mss_scp_bootloader.c,
;; and they *look* like thumb(2?) binaries for a Cortex-M3
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
;; and do NOT generate mv_ddr_build_message.c
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
;; various fixups; the first is that busybox truncate(1) doesn't support '%size'
;; and the second is that marvell.mk has a weird non-PHONY clean rule
;; that is invoked on every build...
(define atf-truncate-fixup #<<EOF
--- a/plat/marvell/a8k/common/a8k_common.mk
+++ b/plat/marvell/a8k/common/a8k_common.mk
@@ -113,7 +113,7 @@ include ${BLE_PATH}/ble.mk
 $(eval $(call MAKE_BL,e))

 mrvl_flash: ${BUILD_PLAT}/${FIP_NAME} ${DOIMAGETOOL} ${BUILD_PLAT}/ble.bin
-	$(shell truncate -s %128K ${BUILD_PLAT}/bl1.bin)
+	$(shell truncate -s 128K ${BUILD_PLAT}/bl1.bin)
 	$(shell cat ${BUILD_PLAT}/bl1.bin ${BUILD_PLAT}/${FIP_NAME} > ${BUILD_PLAT}/${BOOT_IMAGE})
 	${DOIMAGETOOL} ${DOIMAGE_FLAGS} ${BUILD_PLAT}/${BOOT_IMAGE} ${BUILD_PLAT}/${FLASH_IMAGE}

EOF
)

(define atf-mcbin
  (let* ((ver '2.2)
         (src (remote-archive
                (conc "https://github.com/ARM-software/arm-trusted-firmware/archive/v" ver ".tar.gz")
                "Pxwp8bIs5lmYYmedB8SjKH-3bLNV5_1b0TKdAZCGWm4="))
         (mv-ddr-ver 'mv_ddr-armada-atf-mainline)
         (mv-ddr-src (remote-archive
                       ;; TODO: this is not guaranteed to be a stable tarball,
                       ;; but marvell has all but abandoned this code...
                       (conc "https://github.com/MarvellEmbeddedProcessors/mv-ddr-marvell/archive/" mv-ddr-ver ".tar.gz")
                       "nV3OhnbmGNAy4JQz60aYjucT1SrMuoVHppiYbyd40zI="))
         (patches    (patch* atf-truncate-fixup mv-ddr-fixup)))
    (lambda (conf)
      (unless (eq? ($arch conf) 'aarch64)
        (error "atf-mcbin is for aarch64 targets"))
      (make-package
        raw-output: "/boot/flash-image.bin"
        label:  (conc "atf-" ver "-mcbin")
        src:    (append (list src mv-ddr-src mv-ddr-localversion marvell-scp-bl2-blob)
                        patches)
        tools:  (cons libressl (cc-for-target conf #t))
        inputs: (list uboot-mcbin)
        build:  (let ((mflags `(V=1
                                 SCP_BL2=/src/mrvl_scp_bl2.img
                                 PLAT=a80x0_mcbin
                                 ,(conc "BL33=" ($sysroot conf) "/boot/u-boot.bin")))
                      (bflags `(V=1 ,@(splat cc-env/for-kbuild HOSTCC: HOSTLD:)
                                    ,(conc "HOSTCCFLAGS=" (spaced (kref cc-env/for-kbuild HOSTCFLAGS:))))))
                  `((cd ,(conc 'arm-trusted-firmware- ver))
                    (unexport MAKEFLAGS) ;; parallel build is busted
                    (export CROSS_COMPILE ,(conc ($triple conf) "-"))
                    (if ((mkdir drivers/marvell/mv_ddr)))
                    ;; copy mv-ddr-marvell into the ATF source tree;
                    ;; the marvell makefiles expect it to be here
                    (if ((elglob files ,(conc "/mv-ddr-marvell-" mv-ddr-ver "/*"))
                         (cp -r $files drivers/marvell/mv_ddr)))
                    ;; for mv-ddr-localversion:
                    (if ((cp /src/mv_ddr_build_message.c drivers/marvell/mv_ddr/)))
                    ,@(script-apply-patches patches)
                    ;; do not force a clean:
                    (if ((sed "-i" -e "/^mrvl_clean:/,+3d" plat/marvell/marvell.mk)))
                    (if ((sed "-i" -e "s/mrvl_clean//g" plat/marvell/marvell.mk)))
                    ;; the top-level makefile doesn't invoke these sub-makes
                    ;; correctly, so invoke them ahead of time with
                    ;; the right overrides...
                    (if ((make ,@bflags
                               ,(conc "LDLIBS=" (spaced '(-static-pie -lcrypto)))
                               -C tools/fiptool all)))
                    (if ((make ,@bflags
                               ,(conc "DOIMAGE_LD_FLAGS=" (spaced '(-static-pie)))
                               -C tools/marvell/doimage all)))
                    (if ((make ,@mflags "SCP_BL2=/src/mrvl_scp_bl2.img" all fip)))
                    (install -D -m "644" -t /out/boot build/a80x0_mcbin/release/flash-image.bin)))))))

(define (mcbin-sdimage-platform conf kernel)
  (lambda (rootpkgs)
    (uniq-setparts-script
      "mcbin-sdimage-"
      ((config->builder conf)
       atf-mcbin               ;; mmcblk1p1
       (ext2fs "mcbin/boot"
          "14bcce9f-5096-4fa8-bdb6-51b0b12823f1"
          "1G"
          ; contents:
          kernel)              ;; mmcblk1p2
       (squashfs rootpkgs))))) ;; mmcblk1p3

;; mcbin-sdimage is a platform for the Solid Run MacchiatoBin
;; that produces a script for creating a bootable SD card image
;;
;; the SD card must have (at least) 3 partitions; the first two partitions
;; must be 32MB and 1GB, respectively
(define mcbin-sdimage
  (mcbin-sdimage-platform
   (gcc+musl-static-config 'aarch64
			   optflag: '-O2
			   sspflag: '-fstack-protector-strong
			   ;; compiling with -mcpu=... helps us get some
			   ;; additional coverage on our GCC recipe with
			   ;; cross-compilation, because this is an illegal
			   ;; flag for any compiler/architecture except aarch64,
			   ;; so builds will fail if this flag leaks into the
			   ;; wrong CFLAGS arguments
			   extra-cflags: '(-mcpu=cortex-a72))
    (linux/config-static "mcbin" "7T9BGGKMOBpAtgHatOv8gRA2Nf92jDWptxrJLV9T3ms="
                         dtb: 'arch/arm64/boot/dts/marvell/armada-8040-mcbin.dtb)))

