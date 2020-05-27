(import
  scheme
  (only (distill base)
	byacc reflex linux-headers)
  (distill package))

;; note: upstream url is
;; 
;; but we're using the cdn because
;; upstream doesn't provide an HTTPS download URL

(define radvd
  (cmmi-package
   "radvd" "2.18"
   "http://www.litech.org/radvd/dist/$name-$version.tar.gz"
   "gW8OC5o88wIBD9PJYCDtUIG0bX4Zn_VTkkjO2KlLGpI="
   tools: (list byacc reflex)
   libs: (list linux-headers)
   ;; fix compat issues with byacc:
   prepare: '((if ((sed |-i| -e "/YYERROR_VERBOSE/aextern FILE *yyin;"
			-e "s/yyset_in(in);/yyin=in;/"
			-e "s/yylex_destroy();/yyin=NULL;/" gram.y)))
	      (unexport MAKEFLAGS)) ; parallel build issue
   extra-configure: '(--with-pidfile=/run/radvd.pid)))
