(import
 scheme
 (distill base)
 (distill package)

 (pkg libevent)
 (pkg libcap))

(define tor
  (cmmi-package
   "tor" "0.4.6.9"
   "https://dist.torproject.org/$name-$version.tar.gz"
   "53kowrO49w1vy6aHxlNKkOP--iYBQ1p_mRH-Ca1AiwE="
   env: '((ZSTD_CFLAGS . "")
          (ZSTD_LIBS . "-lzstd")
          ;; the configure script makes some assumptions
          ;; when cross-compiling; let's make them explicit:
          (tor_cv_dbl0_is_zero . yes)
          (tor_cv_null_is_zero . yes)
          (tor_cv_malloc_zero_works . yes) ; malloc(0)!=NULL
          (tor_cv_twos_complement . yes))
   libs: (list linux-headers libevent libressl zlib libzstd libcap)
   extra-configure: '(--enable-all-bugs-are-fatal
                      --enable-zstd
                      --disable-manpage
                      --disable-html-manual
                      --disable-asciidoc
                      --disable-systemd
                      --disable-rust)
   ;; config comes from service definition
   cleanup: '(rm -rf /out/etc)))
