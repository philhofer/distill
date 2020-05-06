(import
  scheme
  (only (chicken base) error)
  (only (chicken string) conc)
  (distill memo)
  (distill base)
  (distill plan)
  (distill package))

;; prebuilt go bootstrap binaries
;; (easier than doing the go1.4 C bootstrap,
;; since it doesn't work on all architectures...)
(define go-bootstrap-hashes
  '((x86_64 . "v9N75rm2W-sSWduJ0gxOI4_0X1mD-QGtx_UMikZZj_0=")
    (aarch64 . "CwSnM69pt9tMBSOgV9xvTIggnrr0wiFTnY9vB5RAoF4=")
    (ppc64le . "PWaueP-AiG9w1pDNYPOZ4igLS8YqCcJ7MRW_3VJjjIw=")))

(define go-bootstrap
  (make-meta-package
   (lambda (conf)
     (let ((c (assq ($arch conf) go-bootstrap-hashes)))
       (if c
	   (remote-archive
	    (string-append "https://b2cdn.sunfi.sh/files/pub-cdn/" (cdr c))
	    (cdr c) kind: 'tar.zst)
	   (error "no go-bootstrap for arch" ($arch conf)))))))

(define ($GOARCH conf)
  (case ($arch conf)
    ((aarch64) 'arm64)
    ((armv7 armv6 armv5) 'arm)
    ((x86_64) 'amd64)
    (else ($arch conf))))

(define ($GOEXTRA conf)
  (case ($arch conf)
    ((armv7) '(GOARM . 7))
    ((armv6) '(GOARM . 6))
    ((armv5) '(GOARM . 5))
    ((ppc64le) (cons 'GOPPC64
		     (if (memq '-mcpu=power9 ($CFLAGS conf))
			 'power9
			 'power8)))
    (else #f)))

(define ($go-env conf)
  (let ((tail `((GOROOT . /usr/lib/go)
		(GOOS . linux)
		(GOARCH . ,($GOARCH conf))))
	(extra ($GOEXTRA conf)))
    (if extra (cons extra tail) tail)))

(define go
  (let* ((ver "1.14.2")
	 (src (remote-archive
	       (conc "https://dl.google.com/go/go" ver ".src.tar.gz")
	       "c_Qn6BrbxHUKEeZ7ZdYlUnGUtjqqgXjYmHiTInzjFpU=")))
    (lambda (conf)
      (make-package
       label: (conc "go-" ver "-" ($arch conf))
       dir:   "/go"
       src:   (list src)
       env:   (cons*
	       ;; until we can figure out how to make go
	       ;; cooperate with parallel builds, do serial builds
	       '(GOMAXPROCS . 1)
	       '(GO_GCFLAGS . "-c=1")
	       '(HOME . /tmp/build-home)
	       '(CGO_ENABLED . 0)
	       '(GOROOT_FINAL . /usr/lib/go)
	       ($go-env conf))
       tools: (list go-bootstrap busybox-core execline-tools exportall)
       inputs: '()
       build: '((if ((mkdir -p /tmp/build-home)))
		(if ((cd src)
		     (/bin/ash make.bash)))
		(if ((mkdir -p /out/usr/lib/go/bin /out/usr/bin)))
		(if ((forx tool ((go gofmt)))
		     (importas |-i| -u tool tool)
		     (if ((cp bin/$tool /out/usr/lib/go/bin/$tool)))
		     (ln -s /usr/lib/go/bin/$tool /out/usr/bin/$tool)))
		(if ((cp -a src pkg lib /out/usr/lib/go)))
		(if ((find /out/usr/lib/go/src -type f -name "*_test.go" -delete)))
		(if ((find /out/usr/lib/go/src -type d -name "testdata" -exec rm -rf "{}" "+")))
		(find /out/usr/lib/go/src -type f -name "*.rc" -o -name "*.bat" -delete))))))
