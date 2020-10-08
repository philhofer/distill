(import
  scheme
  (chicken module)
  (distill package))

(export libjq)

(define %jq
  (cmmi-package
   "jq" "1.6"
   "https://github.com/stedolan/$name/releases/download/$name-$version/$name-$version.tar.gz"
   "8CAQoS4P-ztifJlc_ZE0FI0P_Hq8v4aAH0E655zjJ4w="
   extra-configure: '(--enable-all-static
                      --disable-maintainer-mode
                      --disable-docs
                      --with-oniguruma=no)))

(define jq (binaries %jq))
(define libjq (libs %jq))
