(import
 scheme
 (distill package)
 (distill base)
 (distill execline)
 (pkg libevent)
 (pkg libexpat))

(define unbound
  (cmmi-package
   "unbound" "1.15.0"
   "https://unbound.net/downloads/$name-$version.tar.gz"
   "iP_cqu9sLLGaxiF_2AO-AwwSRlGiM7x7y9BGsyPg-lo="
   libs: (list libressl libexpat libevent)
   extra-configure: `(--with-username=unbound
                      --with-run-dir=/etc/unbound
                      --with-chroot-dir=/etc/unbound
                      --with-pidfile=
                      --with-pthreads
                      ,(elconc '--with-ssl= $sysroot '/usr)
                      ,(elconc '--with-libexpat= $sysroot '/usr)
                      --without-pythonmodule
                      --without-pyunbound)
   ;; config comes from service definition
   cleanup: '(rm -rf /out/etc)))
