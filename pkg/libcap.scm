(import
 scheme
 (only (distill memo) cons*)
 (distill package)
 (distill kvector)
 (distill base))

(define libcap
  (let ((src (source-template
	      "libcap" "2.27"
	      "https://kernel.org/pub/linux/libs/security/linux-privs/$name2/$name-$version.tar.xz"
	      "2mB4u6ahspxKFsSPz21FWrHUC0YGDIwT7F_OGCw4RYg=")))
    (lambda (conf)
      (let ((cflags (append ($CFLAGS conf) '(-D_LARGEFILE64_SOURCE -D_FILE_OFFSET_BITS=64)))
	    (cc     ($CC conf))
	    (natenv (cc-toolchain-env ($native-toolchain (build-config)))))
	(source->package
	 conf
	 src
	 tools:  (cons* perl linux-headers (cc-for-target conf #t))
	 inputs: (list linux-headers musl libssp-nonshared)
	 ;; pared-down build for just libcap, not setcap(8) etc.
	 build: `((if ((make -C libcap ,@(k=v* CC: cc CFLAGS: cflags
					       BUILD_CC: (kref natenv CC:)
					       BUILD_CFLAGS: (append (kref natenv CFLAGS:)
								     (list "-I./include" "-I./include/uapi"))
					       AR: ($AR conf) RANLIB: ($RANLIB conf))
			     libcap.a)))
		  (if ((install -D -m "0644" libcap/include/sys/capability.h /out/usr/include/sys/capability.h)))
		  (install -D -m "0644" libcap/libcap.a /out/usr/lib/libcap.a)))))))
