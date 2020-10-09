(import
 scheme
 (distill package))

(define pkgconf
  (cmmi-package
   "pkgconf" "1.7.3"
   "https://distfiles.dereferenced.org/$name/$name-$version.tar.xz"
   "O1NCzLczHDSuYYmse0CyA4L8jcVFs23lXx0EyPDzBn8="
   cleanup: '(ln -sf pkgconf /out/usr/bin/pkg-config)))
