(import
  scheme
  (only (chicken string) conc)
  (distill kvector)
  (distill base)
  (distill plan)
  (only (distill image) libressl squashfs)
  (distill service)
  (distill sysctl)
  (distill net)
  (distill package)
  (distill system)
  (distill fs)
  (distill linux))

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

(define atf-mcbin
  ;; the ATF source used here is vanilla upstream ATF
  ;; plus a vendored copy of marvell-ddr in drivers/marvel/mv-ddr
  ;; and a vendored SCP_BL2 blob (ew)
  ;; and some light Makefile patching to make it compatible with busybox
  ;; (notably, truncate -s %align isn't supported, and CFLAGS needs -fno-PIE,
  ;; and marvell.mk needs some massaging not to run a non-phony clean rule
  ;; on every make invocation... good god.)
  (let* ((ver "65012c0")
         (h   "2tAUr4jElxX8cu-V7NmDstttmBzkytM0SX50JY7Yl8g=")
         (src (remote-archive
                (string-append
                  "https://b2cdn.sunfi.sh/file/pub-cdn/" h)
                h kind: 'tar.zst)))
    (lambda (conf)
      (unless (eq? ($arch conf) 'aarch64)
        (error "atf-mcbin is for aarch64 targets"))
      (make-package
        raw-output: "/boot/flash-image.bin"
        label:  "atf-65012c0-mcbin"
        src:    src
        tools:  (append (native-toolchain-for conf)
                        (cc-for-target conf)
                        (list libressl)) ;; for fiptool
        inputs: (list uboot-mcbin)
        build:  (let ((mflags `(V=1 PLAT=a80x0_mcbin ,(conc "BL33=" ($sysroot conf) "/boot/u-boot.bin")))
                      (bflags `(V=1 ,@(splat cc-env/for-kbuild HOSTCC: HOSTLD:)
                                    ,(conc "HOSTCCFLAGS=" (spaced (kref cc-env/for-kbuild HOSTCFLAGS:))))))
                  (make-recipe
                    script: `((cd arm-trusted-firmware-master)
                              (unexport MAKEFLAGS) ;; parallel build is busted
                              (export CROSS_COMPILE ,(conc ($triple conf) "-"))
                              ;; the top-level makefile doesn't invoke these sub-makes
                              ;; correctly, so invoke them ahead of time with
                              ;; the ride overrides...
                              (if ((make ,@bflags
                                         ,(conc "LDLIBS=" (spaced '(-static-pie -lcrypto)))
                                         -C tools/fiptool all)))
                              (if ((make ,@bflags
                                         ,(conc "DOIMAGE_LD_FLAGS=" (spaced '(-static-pie)))
                                         -C tools/marvell/doimage all)))
                              (if ((make -j1 ,@mflags "SCP_BL2=./mrvl_scp_bl2.img" all fip)))
                              (install -D -m "644" -t /out/boot build/a80x0_mcbin/release/flash-image.bin))))))))

(define (mcbin-sdimage-platform conf kernel)
  (lambda (rootpkgs)
    (uniq-dd-script
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
    (config*
      arch:     'aarch64
      CFLAGS:   '(-pipe -O2 -mcpu=cortex-a72 -fstack-protector-strong)
      CXXFLAGS: '(-pipe -O2 -mcpu=cortex-a72 -fstack-protector-strong))
    (linux/config-static "mcbin" "7T9BGGKMOBpAtgHatOv8gRA2Nf92jDWptxrJLV9T3ms="
                         dtb: 'arch/arm64/boot/dts/marvell/armada-8040-mcbin.dtb)))

