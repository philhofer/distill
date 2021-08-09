(import
 scheme
 (distill package)
 (distill base))

(define tar
  (cmmi-package
   "tar" "1.34"
   "https://ftp.gnu.org/gnu/$name/$name-$version.tar.xz"
   "ghz7FwomEuqMQMrc9I6ryBMNwSmMIM3XiqSe9rcl5Y0="
   env: '((gl_cv_func_gettimeofday_clobber . no)
          (gl_cv_func_tzset_clobber . no))))

