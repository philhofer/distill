(import
 scheme
 (distill package))

(define pkgconf
  (cmmi-package
   "pkgconf" "1.8.0"
   "https://distfiles.dereferenced.org/$name/$name-$version.tar.xz"
   "h_rPUSGygKe3k1uEsBnspmbfHw2EtDnFQ2ser-JJgEs="
   cleanup: '(ln -sf pkgconf /out/usr/bin/pkg-config)))
