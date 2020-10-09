(import
 scheme
 (distill base)
 (distill package))

(define i2c-tools
  (cc-package
   "i2c-tools" "4.1"
   "https://mirrors.edge.kernel.org/pub/software/utils/$name/$name-$version.tar.gz"
   "gRACB_gv_oz_gszxCsu1E1nrPfS5tFHejfNd0VzED3s="
   env: (list $cc-env)
   libs: (list linux-headers)
   build: '(if
            (make DESTDIR=/out PREFIX=/usr BUILD_STATIC_LIB=1 BUILD_DYNAMIC_LIB=0 install)
            rm -rf /out/usr/share)))
