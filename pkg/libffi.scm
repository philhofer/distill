(import
  scheme
  (distill plan)
  (distill base)
  (distill package)
  (distill kvector))

(define libffi
  (let ((src (source-template
	      "libffi" "3.3"
	      "https://github.com/libffi/$name/releases/download/v$version/$name-$version.tar.gz"
	      "D5qxr8A4qKcecQUxZOf_kYJNvwVv64my5oB_bnJyy7M=")))
    (lambda (conf)
      (source->package
       conf
       src
       tools:  (cc-for-target conf)
       inputs: (list musl libssp-nonshared)
       build:  (gnu-recipe
		(kwith
		 ($gnu-build conf)
		 configure-args: (+= '(--disable-docs))))))))
