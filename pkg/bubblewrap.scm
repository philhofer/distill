(import
 scheme
 (distill kvector)
 (distill package)
 (distill base)

 (pkg libcap))

(define bubblewrap
  (let ((src (source-template
	      "bubblewrap" "0.4.1"
	      "https://github.com/containers/$name/releases/download/v$version/$name-$version.tar.xz"
	      "yqpyqB-H9jkcKQyM4vivRReAwzUZGgsNP_HPACJ8gDQ=")))
    (lambda (conf)
      (source->package
       conf
       src
       tools:  (cc-for-target conf)
       inputs: (list libcap linux-headers musl libssp-nonshared)
       build:  (gnu-recipe
		(kwith
		 ($gnu-build conf)
		 configure-args: (+= '(--disable-selinux --disable-man --with-bash-completion-dir=no))))))))
