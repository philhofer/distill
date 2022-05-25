(import
  scheme
  (chicken sort)
  (distill eprint)
  (distill plan)
  (distill package)
  (distill base))

(define (cdn-url c-hash)
  (string-append
   "https://b2cdn.sunfi.sh/file/pub-cdn/"
   c-hash))

;; we're taking the prebuilt tools from
;; the leaf config (via cc-toolchain-tools)
;; and comparing them to what we get from
;; building the tools in default-build-config
(let* ((conf    (force default-build-config))
       (leaf    ($leaf conf))
       (ctools  (o cc-toolchain-tools $cc-toolchain))
       (expn    (expander conf))
       (tools   (append
                 (%execline conf)
                 (%archive-tools conf)
                 (ctools conf)))
       (boots   (ctools leaf))
       (plans   (map expn tools))
       (_       (build-graph! plans))
       (arts    (map plan-outputs plans))
       (arts    (map
                 (lambda (art)
                   (vector-set! art 2 (cdn-url (artifact-hash art)))
                   art)
                 arts))
       (art<?   (lambda (a b)
                  (string<? (artifact-hash a)
                            (artifact-hash b)))))
  (if (equal?
       (sort boots art<?)
       (sort arts art<?))
      (begin
        (info "prebuilts already equivalent"))
      (begin
        (info "outputting new prebuilts")
        (with-output-to-file (string-append "prebuilt-" (symbol->string ($arch conf)) ".scm")
          (lambda ()
            (write (list 'quote arts)))))))
