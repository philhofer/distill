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
   extra-configure: '(--with-seccomp --enable-static)))
