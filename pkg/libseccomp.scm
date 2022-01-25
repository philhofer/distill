(import
 scheme
 (distill base)
 (distill package)
 (pkg gperf))

(define libseccomp
  (cmmi-package
   "libseccomp" "2.5.3"
   "https://github.com/seccomp/$name/releases/download/v$version/$name-$version.tar.gz"
   "4xB5T0mJXWRKzHoumWwstVx9omljGKsQ4b-SCjUkRik="
   libs: (list linux-headers)
   tools: (list gperf)))
