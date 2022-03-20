(import
 scheme
 (distill plan)
 (distill package)
 (distill base)
 (pkg libreadline)
 (pkg ncurses)
 (pkg jansson))

(define nftables
  (cmmi-package
   "nftables" "1.0.2"
   "http://netfilter.org/projects/nftables/files/$name-$version.tar.bz2"
   "rRvbUL6KnLvjdrIzbSQdA7xTFOooMsOrbmYmX6YrJWA="
   env:  '((LIBMNL_LIBS . -lmnl)
           (LIBMNL_CFLAGS . -lmnl)
           (LIBNFTNL_LIBS . -lnftnl)
           (LIBNFTNL_CFLAGS . -lnftnl))
   libs: (list ncurses jansson libgmp libmnl libnftnl linux-headers)
   tools: (list reflex byacc (interned-symlink "/usr/bin/byacc" "/usr/bin/yacc"))
   ;; do not build examples; the #includes won't
   ;; be in the right place for this to work
   prepare: '(sed "-i" -e "s/ examples//" Makefile.in)
   extra-configure: '(--disable-man-doc --without-cli --with-json)))
