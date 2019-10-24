(define-library (plan)
  (export
    artifact?
    art-hash
    art-name
    art-plan
    art-url
    remote-artifact
    plan?
    plan-steps
    plan-inputs
    plan-hash
    artifacts-dfs
    reload-art-hashes!
    save-art-hashes)
  (import (scheme base)
          (scheme write)
          (srfi 2)
          (srfi 9)
	  (srfi 69)
	  (hash))
  (include "plan.scm"))
