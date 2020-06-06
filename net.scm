(define netif-loopback
  (make-service
    name:   'net.lo
    inputs: (list iproute2)
    spec:   (oneshot*
              up:   `(fdmove -c 2 1
			     ip link set dev lo up)
              down: `(fdmove -c 2 1
			     foreground (ip link set dev lo down)
			     true))))
