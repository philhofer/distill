(import
 scheme
 (distill base)
 (distill package))

(define i2c-tools
  (cc-package
   "i2c-tools" "4.3"
   "https://mirrors.edge.kernel.org/pub/software/utils/$name/$name-$version.tar.gz"
   "i8bNkBTDWsBnX304zwSqP_DnsiJsDx9I-vNF34H4McE="
   env: (list $cc-env)
   libs: (list linux-headers)
   build: '(if
            (make DESTDIR=/out PREFIX=/usr BUILD_STATIC_LIB=1 BUILD_DYNAMIC_LIB=0 install)
            rm -rf /out/usr/share)))
