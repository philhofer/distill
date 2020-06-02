(import
  scheme
  (chicken sort)
  (distill eprint)
  (distill plan)
  (distill package)
  (distill base))

(let* ((conf    (force default-build-config))
       (leaf    ($leaf conf))
       (ctools  (o cc-toolchain-tools $cc-toolchain))
       (expn    (expander conf))
       (tools   (ctools conf))
       (boots   (ctools leaf))
       (plans   (map expn tools))
       (_       (build-graph! plans))
       (arts    (map plan-outputs plans))
       (art<? (lambda (a b)
		(string<? (artifact-hash a)
			  (artifact-hash b)))))
  (if (equal?
       (sort boots art<?)
       (sort arts art<?))
      (begin
	(info "prebuilts already equivalent"))
      (begin
	(info "outputting new prebuilts")
	(write (list 'quote arts)))))
