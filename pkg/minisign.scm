(import
  scheme
  (distill base)
  (distill package)

  (pkg cmake)
  (pkg libsodium))

(define minisign
  (cmake-package
   "minisign" "0.8"
   "https://github.com/jedisct1/$name/releases/download/$version/$name-$version.tar.gz"
   "pDmq_F-yzKdaXC25o-o7QtrWRlDtJVl0TnHNI3GcgQ4="
   libs: (list libsodium)))
