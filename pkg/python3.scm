(import
  scheme
  (only (chicken string) conc)
  (distill plan)
  (distill package)
  (distill base)
  (distill kvector)

  (pkg ncurses)
  (pkg libexpat)
  (pkg libffi)
  (pkg tcl)
  (pkg gdbm)
  (pkg libreadline))

(define python3
  (let* ((ver '3.8.2)
         (src (remote-archive
                (conc "https://www.python.org/ftp/python/" ver "/Python-" ver ".tar.xz")
                "Fz7sjZlaw3LqjDO4K3IViMKGscHMEH2_7UTIvSmKxfM=")))
    (lambda (conf)
      (make-package
        label:  (conc "python-" ver "-" ($arch conf))
        src:    src
        tools:  (+cross conf (cc-for-target conf) (list python3))
        inputs: (list
                  musl libssp-nonshared
                  libexpat libressl zlib bzip2 xz-utils
                  libffi tcl linux-headers gdbm libreadline)
        build:  (gnu-recipe
                  (conc "Python-" ver)
                  (kwith ($gnu-build conf)
                         pre-configure: (+= '((export ac_cv_file__dev_ptmx yes)
                                              (export ac_cv_file__dev_ptc no)))
                         configure-args: (+= '(--disable-rpath
                                                --enable-ipv6
                                                --enable-optimizations=no
                                                --with-computed-gotos
                                                --with-dbmliborder=gdbm
                                                --with-system-expat
                                                --with-system-libffi
                                                --with-threads
                                                --without-ensurepip))
                         make-flags: (+= (k=v* EXTRA_CFLAGS: (cons '-DTHREAD_STACK_SIZE=0x100000
                                                                   ($CFLAGS conf))))))))))
