(import
 scheme
 (distill base)
 (distill package))

(define libffi
  (cmmi-package
   "libffi" "3.4.2"
   "https://github.com/libffi/$name/releases/download/v$version/$name-$version.tar.gz"
   "M1XgtdqF0c0rJ_KXx9d34_kOV-peO9RkvChzJG0-zPU="
   libs: (list linux-headers)
   extra-configure: '(--disable-docs)))
