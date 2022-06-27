(import
 scheme
 (distill package)
 (pkg libevent)
 (pkg libseccomp))

(define memcached-tools
  (cmmi-package
   "memcached" "1.6.15"
   "https://memcached.org/files/$name-$version.tar.gz"
   "LTGs2eylvdYFC3P2DoUug_PiYvhYYLDnmpsBPoMs64Q="
   libs: (list libevent libseccomp)
   env: (list '(ac_cv_search_pthread_create . ""))
   override-configure: `(--disable-shared
                         ;; cannot include --enable-static
                         ;; since it implies -static in CFLAGS
                         --prefix=/usr
                         --sysconfdir=/etc
                         --build ,$build-triple
                         --host ,$triple
                         --with-seccomp)))
