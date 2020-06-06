(import
  scheme
  (distill execline)
  (distill plan))

;; ip-wait is a script for waiting
;; until a particular networking-related
;; event has happend (as defined by a pattern
;; occuring on the output of 'ip show' or 'ip monitor')
;;
;; usage:
;;   ip-wait <event-kind> <regex>
;;
;; examples:
;;
;;  # # wait for a globally-routable address:
;;  # ip-wait addr 'inet.*scope global'
;;  # # wait for a default route:
;;  # ip-wait route '^default'
;;  # # wait for wlan0 interface hotplug:
;;  # ip-wait link '^[0-9]+: wlan0:'
;;  # # wait for a neighbor:
;;  # ip-wait neigh 'lladdr aa:bb:cc:dd:ee:ff REACHABLE$'
;;
(define ip-wait
  (interned
   "/bin/ip-wait" #o755
   (lambda ()
     (write-exexpr
      ;; the reason why this is so nasty
      ;; is because there isn't a way to get
      ;; 'ip monitor' to show the current state,
      ;; and because there is a race we need to
      ;; work around between calling 'ip monitor'
      ;; and 'ip ... show' where the event could happen
      '(piperw
	3 4
	background (redirfd
		    -r 0 /dev/null
		    fdclose 3
		    fdmove 1 4
		    ip monitor "$1")
	importas |-i| -u mon !
	fdclose 4
	pipeline (foreground (ip "$1" show)
			     fdmove 0 3
			     cat)
	importas |-i| -u bg !
	if (grep -m1 -E "$2")
	if (kill "$bg")
	if (kill "$mon")
	wait |-i| ("$bg" "$mon"))
      shebang: "#!/bin/execlineb -s2"))))
