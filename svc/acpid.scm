(import
 scheme
 (only (chicken module) export)
 (distill base)
 (distill execline)
 (distill service)
 (distill plan))

;; busybox acpid maps poweroff events
;; to this file, so the simplest default
;; behavior is just to run poweroff(8)
(define acpi-poweroff
  (interned
   "/etc/acpi/PWRF/00000080" #o700
   ;; busybox acpid ignores SIGCHLD,
   ;; which breaks execline scripts,
   ;; so we use /bin/sh which automatically
   ;; resets a reasonable SIGCHLD handler
   "#!/bin/sh\npoweroff\n"))

;; acpid provides ACPI power event handling;
;; currently only the poweroff event is handled
(define acpid
  (make-service
   name:   'acpid
   ;; actual acpid binary provided by busybox-full,
   ;; which is already included in the base system
   inputs: (list acpi-poweroff)
   spec:   (longrun*
            run: `(fdmove -c 2 1 /sbin/acpid -f -d -c /etc/acpi))))
