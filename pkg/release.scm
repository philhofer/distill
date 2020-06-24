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
  (let ((hash    "ahVPTUGm1dD5GFuK-hdPIjBH2pqJtculpto5HrG72gs=")
	($cflags (lambda (conf)
		   ;; due to chicken's compilation model, essentially every
		   ;; continuation is noreturn, so the stack protector does
		   ;; literally nothing
		   (without '(-fstack-protector -fstack-protector-strong -fstack-protector-all)
			    ($CFLAGS conf)))))
    (cc-package
     "distill" "7f62fce"
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
