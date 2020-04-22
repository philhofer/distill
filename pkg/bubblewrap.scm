(import
 scheme
 (distill package)
 (distill base)
 (pkg libcap))

(define bubblewrap
  (cmmi-package
   "bubblewrap" "0.4.1"
   "https://github.com/containers/$name/releases/download/v$version/$name-$version.tar.xz"
   "yqpyqB-H9jkcKQyM4vivRReAwzUZGgsNP_HPACJ8gDQ="
   libs: (list libcap linux-headers)
   extra-configure: '(--disable-selinux --disable-man --with-bash-completion-dir=no)))

