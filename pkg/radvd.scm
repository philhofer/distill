(import
 scheme
 (only (distill base)
       byacc reflex linux-headers)
 (distill package))

(define radvd
  (cmmi-package
   "radvd" "2.19"
   "http://radvd.litech.org/dist/$name-$version.tar.gz"
   "tl0TxVd1tDPDPpG78PU4lBUCLGdKJtnsQELgr0tuPhY="
   tools: (list byacc reflex)
   libs: (list linux-headers)
   ;; fix compat issues with byacc:
   prepare: '(sed |-i| -e "/YYERROR_VERBOSE/aextern FILE *yyin;"
                  -e "s/yyset_in(in);/yyin=in;/"
                  -e "s/yylex_destroy();/yyin=NULL;/" gram.y)
   extra-configure: '(--with-pidfile=/run/radvd.pid)))
