(import
 scheme
 (chicken string)
 (distill base)
 (distill plan)
 (distill execline)
 (distill package))

(define vector-prebuilt-src
  (lambda (conf)
    (let ((arch ($arch conf)))
      (remote-archive
       (string-translate*
        "https://packages.timber.io/vector/$version/vector-$arch-unknown-linux-musl.tar.gz"
        `(("$arch" . ,(symbol->string arch))
          ("$version" . "0.22.2")))
       "k-McBdbA1RbzF_ma9tVMXLlh3_NovxwTAsg-ycBWBZQ="))))


(define vector-prebuilt
  (package-template
   label:  "vector-prebuilt"
   inputs: (list vector-prebuilt-src)
   tools:  (list busybox-core) ;; need install(1)
   build: `(install -D
                    ,(elconc $sysroot "/vector-" $arch "-unknown-linux-musl/bin/vector")
                    /out/bin/vector)))
