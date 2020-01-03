(import
  scheme
  (scheme load)
  (chicken process-context)
  (execline)
  (hash)
  (filepath)
  (log)
  (plan))

;; this is a hack to prevent additional
;; (import ...) statements for execline/hash/filepath/log
;; from having to load any code, since the import library
;; contains the syntactic definitions that we would otherwise
;; need to load when expanding other code
(begin
  (include "table.import.scm")
  (include "execline.import.scm")
  (include "hash.import.scm")
  (include "filepath.import.scm")
  (include "log.import.scm")
  (include "plan.import.scm"))

(for-each load (command-line-arguments))
