(import
 scheme
 (distill package)
 (distill base))

(define tar
  (cmmi-package
   "tar" "1.32"
   "https://ftp.gnu.org/gnu/$name/$name-$version.tar.xz"
   "yevL-uqj4F98n_dVcnVzCc6F6jEGPn2IqXZqH28Y3Go="
   env: '((gl_cv_func_gettimeofday_clobber . no)
          (gl_cv_func_tzset_clobber . no))))

