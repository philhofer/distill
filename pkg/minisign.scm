(import
 scheme
 (distill base)
 (distill package)

 (pkg cmake)
 (pkg libsodium)
 (pkg pkgconf))

(define minisign
  (cmake-package
   "minisign" "0.9"
   "https://github.com/jedisct1/$name/releases/download/$version/$name-$version.tar.gz"
   "zC79CR55UEG6GqNshy85flENdK6BfxDdYCdZ6glrBhc="
   tools: (list pkgconf)
   libs: (list libsodium)))
