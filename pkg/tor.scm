(import
 scheme
 (distill base)
 (distill package)
 (distill plan)
 (distill execline)
 (pkg libevent)
 (pkg libcap))

(define libressl-patches
  (map
   (lambda (lst)
     (let ((name (car lst))
           (uri  (cadr lst))
           (hash (caddr lst)))
       (remote-file
        uri hash
        (string-append "/src/patch-" name ".patch")
        #o644)))
   '(("0" "https://raw.githubusercontent.com/openbsd/ports/e81b3cdf62fbb325e4fd1b60c731858a1221a576/net/tor/patches/patch-src_lib_crypt_ops_crypto_rsa_openssl_c" "Wj_eIH72qWOVnXpZrz2Y1vd2NY1fRFqbGdhlb49gcZQ=")
     ("1" "https://raw.githubusercontent.com/openbsd/ports/e81b3cdf62fbb325e4fd1b60c731858a1221a576/net/tor/patches/patch-src_lib_tls_x509_openssl_c" "0wzP31Fvvm6l9b4mCin5qtbNHFp4wFcKhRXg5frbAfA=")
     ("2" "https://raw.githubusercontent.com/openbsd/ports/e81b3cdf62fbb325e4fd1b60c731858a1221a576/net/tor/patches/patch-src_lib_crypt_ops_crypto_dh_openssl_c" "LTDg74FtU_0ulq7qvCHAuXVwz2TXgqptI0VDJgB1Wos=")
     ("3" "https://raw.githubusercontent.com/openbsd/ports/e81b3cdf62fbb325e4fd1b60c731858a1221a576/net/tor/patches/patch-src_test_test_crypto_c" "mL8nEwB2-ipvlIH1CGYV7hGl7VW3kZnQhh2jKj3j8Bs=")
     ("4" "https://raw.githubusercontent.com/openbsd/ports/e81b3cdf62fbb325e4fd1b60c731858a1221a576/net/tor/patches/patch-src_test_test_crypto_openssl_c" "cHZlPlzV1stmwWhP8zT59u0pMogytHhIebyF4sn8qH0="))))

(define tor
  (cmmi-package
   "tor" "0.4.6.10"
   "https://dist.torproject.org/$name-$version.tar.gz"
   "AXRipP2u8tno2oJY_S-UXmjufSPb0_ecqjd9Ke1_er8="
   env: '((ZSTD_CFLAGS . "")
          (ZSTD_LIBS . "-lzstd")
          ;; the configure script makes some assumptions
          ;; when cross-compiling; let's make them explicit:
          (tor_cv_dbl0_is_zero . yes)
          (tor_cv_null_is_zero . yes)
          (tor_cv_malloc_zero_works . yes) ; malloc(0)!=NULL
          (tor_cv_twos_complement . yes))
   tools: (list libressl-patches)
   libs: (list linux-headers libevent libressl zlib libzstd libcap)
   prepare: `(pipeline
              (cat ,@(map (lambda (num)
                           (string-append "/src/patch-" (number->string num) ".patch"))
                         '(0 1 2 3 4)))
              pipeline (sed "s/\\.orig$//")
              patch -p0)
   extra-configure: '(--enable-all-bugs-are-fatal
                      --enable-zstd
                      --disable-manpage
                      --disable-html-manual
                      --disable-asciidoc
                      --disable-systemd
                      --disable-rust)
   ;; config comes from service definition
   cleanup: '(rm -rf /out/etc)))
