(define netif-loopback
  (make-service
    name:   'net.lo
    inputs: (list iproute2)
    spec:   (oneshot*
              up:   `((fdmove -c 2 1)
                      (ip link set dev lo up))
              down: `((fdmove -c 2 1)
                      (foreground ((ip link set dev lo down)))
                      (true)))))

(define (el-null-or x)
  (if (null? x) '() `((if (,x)))))

(define (el-end-or expr rest)
  (if (null? rest)
    (list expr)
    `((foreground (,expr)) ,@rest)))

(define (netif name #!key
               (addrs     '())
               (pre-up    '())
               (post-up   '())
               (pre-down  '())
               (post-down '())
               (after     '()))
  (make-service
    name:   (string->symbol (conc "net." name))
    inputs: (list iproute2)
    after:  after
    spec:   (oneshot*
              up:   `((fdmove -c 2 1)
                      ,@(map
                          (lambda (addr)
                            `(ip address add ,addr dev ,name))
                          addrs)
                      ,@(el-null-or pre-up)
                      ,@(el-end-or `(ip link set dev ,name up) post-up))
              down: `((fdmove -c 2 1)
                      ,@(el-null-or pre-down)
                      ,@(el-end-or `(ip link set dev ,name down) post-down)))))
