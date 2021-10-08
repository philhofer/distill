(import
 scheme
 (only (chicken string) conc)
 (distill plan)
 (distill package)
 (distill base)
 (distill kvector)
 (distill execline)

 (pkg ncurses)
 (pkg libexpat)
 (pkg libffi)
 (pkg tcl)
 (pkg gdbm)
 (pkg libreadline))

;; this py-setup arranges for just about
;; all of the modules to be built-in statically
(define python-config
  (cdn-artifact "6iCCIatqDyPAUvIMW9CiVQG_vP-oBZxzL-RLAOuX050=" "/src/py-setup" #o644))

(define python3
  (cmmi-package
   "Python" "3.9.6"
   "https://www.python.org/ftp/python/$version/$name-$version.tar.xz"
   "Orxtc7Mb8vFdp4nveaojbC7xpLlXs978TAjf55Iihwg="
   extra-src: (list python-config)
   libs: (list ncurses libexpat libressl zlib libbz2 liblzma libffi
               linux-headers gdbm libreadline)
   cross: (list
           (lambda (conf)
             ;; TODO: figure out precisely the conditions under which
             ;; the build script requires an external python3 program;
             ;; right now we're approximating it as 'build-triple != host-triple'
             (if (eq? ($triple conf) ($build-triple conf)) '() (list python3))))
   env:   '((ac_cv_file__dev_ptmx . yes)
            (ac_cv_file__dev_ptc . no))
   prepare: '(cp /src/py-setup Modules/Setup)
   cleanup: '(pipeline (find /out -type d -name __pycache__ -print0)
                       xargs "-0" rm -rf)
   extra-configure: `(,(elconc '--with-openssl= $sysroot '/usr)
                      --enable-ipv6
                      --enable-optimizations=no
                      --with-computed-gotos
                      --with-dbmliborder=gdbm
                      --with-system-expat
                      --without-ensurepip)
   override-make: (let (($py-cflags (lambda (conf)
                                      (cons '-DTHREAD_STACK_SIZE=0x100000 ($CFLAGS conf)))))
                    `(,(el= 'EXTRA_CFLAGS= $py-cflags) ,$make-overrides))))
