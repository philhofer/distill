(import
  scheme
  (distill base)
  (distill package))

(define libffi
  (cmmi-package
   "libffi" "3.3"
   "https://github.com/libffi/$name/releases/download/v$version/$name-$version.tar.gz"
   "D5qxr8A4qKcecQUxZOf_kYJNvwVv64my5oB_bnJyy7M="
   extra-configure: '(--disable-docs)))
