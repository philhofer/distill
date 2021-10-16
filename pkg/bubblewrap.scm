(import
 scheme
 (distill package)
 (distill base)
 (pkg libcap))

(define bubblewrap
  (cmmi-package
   "bubblewrap" "0.5.0"
   "https://github.com/containers/$name/releases/download/v$version/$name-$version.tar.xz"
   "wnuJ6h4Fx4-ZQ8ZBsJ3a5pWCdCwO4-R1sh5awvDxpZM="
   libs: (list libcap linux-headers)
   extra-configure: '(--disable-selinux
                      --disable-man
                      --with-bash-completion-dir=no
                      --with-zsh-completion-dir=no)))
