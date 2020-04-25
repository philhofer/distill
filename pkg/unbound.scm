(import
 scheme
 (distill package)
 (distill base)
 (pkg libevent)
 (pkg libexpat))

(define unbound
  (cmmi-package
   "unbound" "1.10.0"
   "https://unbound.net/downloads/$name-$version.tar.gz"
   "Q9l5kQXkBj0bED0ZRDze_OX4ahUlJYBElK7Mnrse6uY="
   libs: (list libressl libexpat libevent)
   extra-configure: (vargs
		     `(--with-username=unbound
		       --with-run-dir=/etc/unbound
		       --with-chroot-dir=/etc/unbound
		       --with-pidfile=
		       --with-pthreads
		       (--with-ssl= ,$sysroot /usr)
		       (--with-libexpat= ,$sysroot /usr)
		       --without-pythonmodule
		       --without-pyunbound))
   ;; config comes from service definition
   cleanup: '((if ((rm -rf /out/etc))))))
