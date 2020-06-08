(import
  scheme
  (distill package))

(define pkgconf
  (cmmi-package
   "pkgconf" "1.1.0"
   "https://distfiles.dereferenced.org/$name/$name-$version.tar.xz"
   "Gz7Gt_OuMI4GXzuBsAnhjSQCdjZG7gLqY8rxCPze-AI="
   cleanup: '(ln -sf pkg-config /out/usr/bin/pkgconf)))
