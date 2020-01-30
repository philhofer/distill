(foreign-declare "#include <unistd.h>")

(define (nproc)
  (let* ((sc  (foreign-lambda* long ()
                "C_return(sysconf(_SC_NPROCESSORS_ONLN));"))
         (res (sc)))
    (if (> res 0)
      res
      (error "sysconf(_SC_NPROCESSORS_ONLN) returned" res))))
