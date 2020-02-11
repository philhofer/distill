(include "sequence.mod.scm")
(import
  scheme
  (distill sequence))

(include "test-helpers.scm")
(include "sysctl.scm")

(define *sysctls*
  '((net (ipv4 (tcp_syncookies 1)
               (conf (default rp_filter 1)
                     (all     (rp_filter 1)
                              (accept_redirects 0)
                              (secure_redirects 1)))
               (tcp_rfc1337 1))
         (ipv6 (conf (default use_tempaddr 2))
               (all  (accept_redirects 0)
                     (use_tempaddr 2))))
    (fs (protected_hardlinks 1)
        (protected_symlinks  1))
    (kernel yama ptrace_scope 2)))

(define want #<<EOF
net.ipv4.tcp_syncookies = 1
net.ipv4.conf.default.rp_filter = 1
net.ipv4.conf.all.rp_filter = 1
net.ipv4.conf.all.accept_redirects = 0
net.ipv4.conf.all.secure_redirects = 1
net.ipv4.tcp_rfc1337 = 1
net.ipv6.conf.default.use_tempaddr = 2
net.ipv6.all.accept_redirects = 0
net.ipv6.all.use_tempaddr = 2
fs.protected_hardlinks = 1
fs.protected_symlinks = 1
kernel.yama.ptrace_scope = 2

EOF
)

(test string=? want (sysctls->string *sysctls*))
