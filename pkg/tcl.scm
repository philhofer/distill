(import
  scheme
  (only (chicken string) conc)
  (distill package)
  (distill base)
  (distill kvector))

(define ($64bit-flag conf)
  (if (memq ($arch conf) '(x86_64 ppc64le ppc64 aarch64))
      '--enable-64bit
      '--disable-64bit))

(define tcl
  (let ((ver "8.6.10")
	($make-overrides (lambda (conf)
			   (kupdate ($make-overrides conf)
				    RANLIB: "/bin/true"))))
    (cmmi-package
     "tcl" ver
     "https://prdownloads.sourceforge.net/$name/$name$version-src.tar.gz"
     "93t9GAoB4OZrqtf3rgVDhSNHWJ8uhNe4mBBITBU9POU="
     env: '((tcl_cv_strtoul_unbroken . ok))
     dir: (string-append "tcl" ver "/unix")
     libs: (list zlib)
     tools: (lambda (conf)
	      (if (eq? ($triple conf) ($build-triple conf))
		  '()
		  (list tcl)))
     ;; insert missing #include in generic/tcl.h
     prepare: '(sed "-i" -e "20a #include <sys/stat.h>\n" "../generic/tcl.h")
     override-make: (list $make-overrides)
     extra-configure: `(--enable-shared=no ; doesn't always respect disable-shared
			--mandir=/usr/share/man
			--disable-load
			,$64bit-flag))))
