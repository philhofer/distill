(import
  scheme
  (distill plan)
  (distill package)
  (distill execline)
  (pkg chicken))

(define (without syms lst)
  (if (null? lst)
      lst
      (let ((head (car lst)))
	(if (memq head syms)
	    (cdr lst)
	    (cons head (without syms (cdr lst)))))))

(define release
  (let ((hash    "0lJoG9e0WHdBM1xxaCjQkoelX6Kw_1P9X37h8iAZhO4=")
	($cflags (lambda (conf)
		   ;; due to chicken's compilation model, essentially every
		   ;; continuation is noreturn, so the stack protector does
		   ;; literally nothing
		   (without '(-fstack-protector -fstack-protector-strong -fstack-protector-all)
			    ($CFLAGS conf)))))
    (cc-package
     "distill" "c5f3143"
     (string-append
      "https://b2cdn.sunfi.sh/files/pub-cdn/" hash)
     hash
     tools:  (list chicken)
     build:  `(make DESTDIR=/out
		PREFIX=/usr
		CSI=/usr/bin/csi
		CHICKEN=/usr/bin/chicken
		,(el= 'CHICKEN_FEATURES= $chicken-features)
		,(el= 'AR= $AR)
		,(el= 'CC= $CC)
		,(el= 'CFLAGS= $cflags)
		,(el= 'LDFLAGS= $LDFLAGS)
		install))))
