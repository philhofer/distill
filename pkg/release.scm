(import
 scheme
 (distill base)
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
  (let ((hash "Id7BUW-Zw9XX8aWJiTUxelmZ_fjiyicmn3SVD-6VUXE=")
        ($cflags (lambda (conf)
                   ;; due to chicken's compilation model, essentially every
                   ;; continuation is noreturn, so the stack protector does
                   ;; literally nothing
                   (without '(-fstack-protector -fstack-protector-strong -fstack-protector-all)
                            ($CFLAGS conf)))))
    (cc-package
     "distill" "6020b61"
     (string-append
      "https://b2cdn.sunfi.sh/file/pub-cdn/" hash)
     hash
     tools:  (list chicken)
     inputs: (list libzstd libarchive)
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
