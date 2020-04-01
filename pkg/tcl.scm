(import
  scheme
  (only (chicken string) conc)
  (distill plan)
  (distill package)
  (distill base)
  (distill kvector))

(define tcl
  (let* ((ver '8.6.10)
         (src (remote-archive
                (conc "https://prdownloads.sourceforge.net/tcl/tcl" ver "-src.tar.gz")
                "93t9GAoB4OZrqtf3rgVDhSNHWJ8uhNe4mBBITBU9POU=")))
    (lambda (conf)
      (make-package
        label:  (conc "tcl-" ver "-" ($arch conf))
        src:    src
        tools:  (+cross conf
                        (cc-for-target conf)
                        ; for cross-compilation, we need
                        ; a native version of tclsh
                        (list tcl))
        inputs: (list zlib musl libssp-nonshared)
        build:  (gnu-recipe
                  (conc "tcl" ver "/unix")
                  (kwith
                    ($gnu-build
                      (kwith conf RANLIB: (:= '/bin/true)))
                    pre-configure: (+= '((export tcl_cv_strtoul_unbroken ok)
                                         (if ((sed "-i" -e "20a #include <sys/stat.h>\n" "../generic/tcl.h")))))
                    configure-args: (+= `(--enable-shared=no ; doesn't always respect disable-shared
                                          --mandir=/usr/share/man
                                           --disable-load
                                           ,(if (memq ($arch conf) '(x86_64 ppc64le ppc64 aarch64))
                                              '--enable-64bit
                                              '--disable-64bit)))))))))
