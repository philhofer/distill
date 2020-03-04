(define libmnl
  (let* ((version '1.0.4)
         (src     (remote-archive
                    (conc "https://netfilter.org/projects/libmnl/files/libmnl-" version ".tar.bz2")
                    "kUOLXuIdscWD_5WHBvAIuytyuy-gGm_LXen3TWodgNs=")))
    (lambda (conf)
      (make-package
        label:   (conc "libmnl-" version "-" ($arch conf))
        src:     src
        tools:   (cc-for-target conf)
        inputs:  (list linux-headers musl libssp-nonshared)
        build:   (gnu-recipe
                   (conc "libmnl-" version)
                   ($gnu-build conf))))))

(define libnftnl
  (let* ((version '1.1.5)
         (src     (remote-archive
                    (conc "https://netfilter.org/projects/libnftnl/files/libnftnl-" version ".tar.bz2")
                    "R3Dq9wvV2TNUcXSy_KDTNcR4G8fm_ypLB9xLc0OfEBc=")))
    (lambda (conf)
      (let ()
        (make-package
          label:  (conc "libnftnl-" version "-" ($arch conf))
          src:    src
          tools:  (cc-for-target conf)
          inputs: (list linux-headers libmnl musl libssp-nonshared)
          build:  (gnu-recipe
                    (conc "libnftnl-" version)
                    (kwith
                      ($gnu-build conf)
                      ;; LIBMNL_CFLAGS needs to be set
                      ;; to make the configure script happy,
                      ;; but there isn't actually a value
                      ;; we'd like to set...
                      configure-args: (+= '(LIBMNL_CFLAGS=-lmnl
                                              LIBMNL_LIBS=-lmnl))
                      make-flags: (+= '(V=1)))))))))

(define iptables
  (let* ((version '1.8.4)
         (src     (remote-archive
                    (conc "https://netfilter.org/projects/iptables/files/iptables-" version ".tar.bz2")
                    "hBWYiIU2PYebYoMF6_n_anAFXGfruGBmAXU94ge9DQo=")))
    (lambda (conf)
      (let ((conf (kwith conf CFLAGS: (+= '(-D_GNU_SOURCE)))))
        (make-package
          label:  (conc "iptables-" version "-" ($arch conf))
          src:    src
          tools:  (append (list byacc reflex) (cc-for-target conf))
          inputs: (list linux-headers libnftnl libmnl musl libssp-nonshared)
          build:  (gnu-recipe
                    (conc "iptables-" version)
                    (kwith
                      ($gnu-build conf)
                      configure-args: (+= '(libmnl_CFLAGS=-lmnl
                                              libmnl_LIBS=-lmnl
                                              libnftnl_CFLAGS=-lnftnl
                                              libnftnl_LIBS=-lnftnl)))))))))

(define iproute2
  (let* ((version '5.5.0)
         (src     (remote-archive
                    (conc "https://kernel.org/pub/linux/utils/net/iproute2/iproute2-" version ".tar.xz")
                    "zVeW6PtWecKE9Qlx9l4NrfnQcIZNAW4HocbzuLiJOpo="))
         (patch0  (remote-file
                    "https://git.alpinelinux.org/aports/plain/main/iproute2/musl-fixes.patch"
                    "K4srcIY08guTgXv7DeGR6InxsXUKFV76vmeLao7Y0Cw="
                    "/src/musl-fixes.patch"
                    #o644))
         (patch1  (remote-file
                    "https://git.alpinelinux.org/aports/plain/main/iproute2/fix-install-errors.patch"
                    "jUzhNv5c3_lyQZ6omNKQaBvZNbpHZVkyeMuG15uq1sA="
                    "/src/fix-install-errors.patch"
                    #o644)))
    (lambda (conf)
      ;; the configure script isn't autoconf and
      ;; doesn't work without pkgconfig, but luckily
      ;; all it does is generate config.mk, so just do that directly...
      (let ((config.mk (interned
                         "/src/config.mk"
                         #o644
                         (lines/s
                           (list->seq
                             (append
                               '("YACC=yacc")
                               (splat
                                 conf
                                 CC: LDFLAGS: AR:)
                               (list
                                 "TC_CONFIG_IPSET:=y"
                                 "TC_CONFIG_NO_XT:=y"
                                 "HAVE_MNL:=y"
                                 "CFLAGS += -DHAVE_ELF -DHAVE_SETNS -DHAVE_LIBMNL"
                                 "LDLIBS += -lelf -lmnl -lz"
                                 "%.o: %.c"
                                 "\t$(CC) $(CFLAGS) -c -o $@ $<")))))))
        (make-package
          label: (conc "iproute2-" version "-" ($arch conf))
          src:   (list src patch0 patch1 config.mk)
          tools: (append (list byacc reflex)
                         (cc-for-target conf))
          inputs: (list linux-headers iptables libmnl libelf zlib musl libssp-nonshared)
          build:  (make-recipe
                    script: (execline*
                              (cd ,(conc "iproute2-" version))
                              (if ((cp /src/config.mk config.mk)))
                              ,@(script-apply-patches (list patch0 patch1))
                              (if ((sed "-i" -e "/^SUBDIRS/s: netem::" Makefile)))
                              (if ((make ,@(k=v* CCOPTS: ($CFLAGS conf))
                                         SHARED_LIBS=n PREFIX=/usr all)))
                              (if ((make SHARED_LIBS=n DESTDIR=/out PREFIX=/usr install)))
                              (if ((rm -rf /out/usr/share/bash-completion)))
                              (if ((rm -rf /out/var)))
                              (if ((rm -rf /out/usr/share/man)))
                              ,@(strip-binaries-script ($triple conf)))))))))

(define netif-loopback
  (make-service
    name:   'net.lo
    inputs: (list iproute2)
    spec:   (oneshot*
              up:   (execline*
                      (fdmove -c 2 1)
                      (ip link set dev lo up))
              down: (execline*
                      (fdmove -c 2 1)
                      (foreground ((ip link set dev lo down)))
                      (true)))))

(define (el-null-or x)
  (if (null? x) '() `((if (,x)))))

(define (el-end-or expr rest)
  (if (null? rest)
    (list expr)
    `((if (,expr)) ,@rest)))

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
    after:  (as list? after)
    spec:   (oneshot*
              up:   (execline*
                      (fdmove -c 2 1)
                      ,@(map
                          (lambda (addr)
                            `(ip address add ,addr dev ,name))
                          addrs)
                      ,@(el-null-or pre-up)
                      ,@(el-end-or `(ip link set dev ,name up) post-up))
              down: (execline*
                      (fdmove -c 2 1)
                      ,@(el-null-or pre-down)
                      ,@(el-end-or `(ip link set dev ,name down) post-down)))))

