(import
 scheme
 (distill service)
 (distill unix)
 (pkg memcached-tools))

;; (memcached . args) is a simple memcached service
;; definition that runs 'memcached args...' as the
;; complete command line for the binary
(define (memcached . args)
  (make-service
   name:   'memcached
   users:  (list (adduser 'memcached group: 'memcached))
   groups: (list (addgroup 'memcached '(memcached)))
   inputs: (list memcached-tools)
   spec:   (longrun*
            run: `(fdmove -c 2 1 memcached -u memcached ,@args))))
