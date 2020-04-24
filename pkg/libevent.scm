(import
 scheme
 (distill package)
 (distill base))

(define libevent
  (cmmi-package
   "libevent" "2.1.11-stable"
   "https://github.com/libevent/$name/releases/download/release-$version/$name-$version.tar.gz"
   "ZWr7Vj5fT7U8g7oYAXQoLDQOyzfe57lLuEl8Y_gc-Ig="
   libs: (list libressl)))
