(define (sysctl->text lst)
  (lambda (kons seed)
    (let loop ((out seed)
               (lst lst)
               (str ""))
      (if (null? lst)
        out
        (let ((head (car lst))
              (rest (cdr lst)))
          (cond
            ((pair? head)
             (loop (loop out head str)
                   rest
                   str))
            ((null? rest)
             (kons
               (string-append str " = "
                              (cond
                                ((number? head)  (number->string head))
                                ((symbol? head)  (symbol->string head))
                                ((string? head)  head)
                                ((boolean? head) (if head "1" "0"))
                                (else (error "malformed sysctl" lst))))
               out))
            ((symbol? head)
             (loop out (cdr lst)
                   (if (string=? str "")
                     (symbol->string head)
                     (string-append str "." (symbol->string head)))))
            (else (error "malformed sysctl" lst))))))))

(define *default-sysctls*
  '((net (core bpf_jit_harden 2)
         (ipv4 (tcp_syncookies 1)
               (tcp_rfc1337 1)
               (conf (default rp_filter 1)
                     (all     (rp_filter 1)
                              (accept_redirects 0)
                              (secure_redirects 1))))
         (ipv6 (conf (default use_tempaddr 2))
               (all  (accept_redirects 0)
                     (use_tempaddr 2))))
    (fs (protected_hardlinks 1)
        (protected_fifos     1)
        (protected_symlinks  1))
    (kernel (yama ptrace_scope 2)
            (dmesg_restrict 1)
            (panic 10)
            (panic_on_io_nmi 1)
            (panic_on_stackoverflow 1)
            (panic_on_oops 1))))

(define (sysctls->string lst)
  (lines/s (s/bind (list->seq lst)
                   (kompose (k/map sysctl->text) k/recur))))

(define (sysctl-service name sysctls #!key (after '()))
  (let* ((file        (string-append "/etc/sysctl.d/" name ".conf"))
         (sysctl-file (interned file #o644 (sysctls->string sysctls))))
    (make-service
      name:   (string->symbol name)
      inputs: (list sysctl-file)
      after:  after
      spec:   (oneshot*
                up: `((fdmove -c 2 1)
                      (/sbin/sysctl -p ,file))))))

(define default-sysctls
  (sysctl-service "sysctl.default" *default-sysctls*))

