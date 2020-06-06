;; sysctls have the following grammar:
;;
;;  teriminal = number | symbol | string | boolean
;;  component = (symbol+ terminal) | (symbol component-list+)
;;  component-list = (component)+
;;
;; component-lists are used as a short-hand for sysctls
;; that share prefixes; every component-list within
;; a component is implicitly prefixed by every symbol
;; that appears before it
;;
;; for example:
;;  (net core bpf_jit_harden 2)
;;  (net ipv4 tcp_syncookies 1)
;;  (net ipv4 tcp_rfc1337 1)
;; is equivalent to:
;;  (net (core bpf_harden 2)
;;       (ipv4 (tcp_syncookies 1)
;;             (tcp_rfc1337    1)))
;;
;; each-sysctl-text calls 'proc'
;; for each string that ought to
;; appear as textual lines
(define (each-sysctl-text proc lst)
  (let loop ((lst lst)
	     (str ""))
    (or (null? lst)
        (let ((head (car lst))
              (rest (cdr lst)))
          (cond
	   ((pair? head)
	    (begin
	      (loop head str)
	      (loop rest str)))
	   ((null? rest)
	    (proc
	     (string-append str " = "
			    (cond
			     ((number? head)  (number->string head))
			     ((symbol? head)  (symbol->string head))
			     ((string? head)  head)
			     ((boolean? head) (if head "1" "0"))
			     (else (error "malformed sysctl" lst))))))
	   ((symbol? head)
	    (loop (cdr lst)
		  (if (string=? str "")
		      (symbol->string head)
		      (string-append str "." (symbol->string head)))))
	   (else (error "malformed sysctl" lst)))))))

(define (sysctls->string lst)
  (let ((line (lambda (c)
		(display c) (newline))))
    (with-output-to-string
      (lambda ()
	(each-sysctl-text line lst)))))

;; sysctl-service creates a service with the given name
;; that toggles the provided sysctl spec
(define (sysctl-service name sysctls #!key (after '()))
  (let* ((file        (string-append "/etc/sysctl.d/" name ".conf"))
         (sysctl-file (interned file #o644 (sysctls->string sysctls))))
    (make-service
      name:   (string->symbol name)
      inputs: (list sysctl-file)
      after:  after
      spec:   (oneshot*
                ;; note: this configuration is noisy
                ;; (it will log every sysctl line),
                ;; but that seems okay given that it
                ;; is likely going to the standard logger
                up: `(fdmove -c 2 1
			     /sbin/sysctl -p ,file)))))

;; generic hardening sysctls; suitable for most systems
(define default-sysctls
  (sysctl-service
    "sysctl.default"
    '((net (core bpf_jit_harden 2)
           ;; note: these tcp sysctls apply to ipv6 as well
           (ipv4 (tcp_syncookies 1)
                 (tcp_rfc1337 1)
                 (conf (default rp_filter 1)
                       (all     (rp_filter 1)
                                (accept_redirects 0)
                                (secure_redirects 1))))
           (ipv6 conf all use_tempaddr 2))
      (fs (protected_hardlinks 1)
          (protected_fifos     1)
          (protected_symlinks  1))
      (kernel (yama ptrace_scope 2)
              (dmesg_restrict 1)
              (panic 10)
              (panic_on_io_nmi 1) ;; doesn't always exist
              (panic_on_oops 1)))))
