(define-type stringy (or string symbol integer))

(: stringify (stringy --> string))
(define (stringify x)
  (cond
    ((string? x) x)
    ((symbol? x) (symbol->string x))
    ((integer? x) (number->string x 10))
    (else (error "can't stringify" x))))

(: dirname (string --> string))
(define (dirname str)
  (let ((end (- (string-length str) 1)))
    (let loop ((i end))
      (cond
        ((<= i 0) (if (eqv? (string-ref str 0) #\/) "/" "."))
        ((char=? (string-ref str i) #\/) (##sys#substring str 0 i))
        (else (loop (- i 1)))))))

;; see srfi 13
(: string-prefix? (string string --> boolean))
(define (string-prefix? pre str)
  (let ((plen (string-length pre))
	(slen (string-length str)))
    (and (<= plen slen)
	 (substring=? pre str 0 0 plen))))

;; see srfi 13
(: string-suffix? (string string --> boolean))
(define (string-suffix? suff str)
  (let ((elen (string-length suff))
	(slen (string-length str)))
    (and (<= elen slen)
	 (substring=? str suff (- slen elen) 0 elen))))

(: basename (string --> string))
(define (basename str)
  (let ((end (string-length str)))
    (let loop ((i (- end 1)))
      (cond
        ((< i 0) str)
        ((char=? (string-ref str i) #\/) (##sys#substring str (+ i 1) end))
        (else (loop (- i 1)))))))

(define (strsep proc str sep seed)
  (let* ((end  (string-length str))
	 (scan (lambda (start)
		 (let loop ((i start))
		   (cond
		    ((>= i end) end)
		    ((char=? (string-ref str i) sep) i)
		    (else (loop (+ i 1))))))))
    (let loop ((i 0)
	       (v seed))
      (if (>= i end)
          v
          (let ((seg (scan i)))
            (loop (+ seg 1) (proc str i seg v)))))))

;; core filepath normalization routine
;;
;; join one or more filepath components together
;; while eliminating '.' and '..' components where possible
(: filepath-join (stringy #!rest stringy --> string))
(define filepath-join
  ;; allocate the inner reducing function closure just once
  ;; (these produce a single-pass reducer from path components
  ;; to output path string)
  (let* ((seg=?     (lambda (str start end seg)
		      (let ((strlen (- end start))
			    (seglen (string-length seg)))
			(and (= strlen seglen)
			     (substring=? str seg start 0 strlen)))))
	 (fold      (lambda (proc init lst)
		      (let loop ((out init) (lst lst))
			(if (null? lst) out (loop (proc (car lst) out) (cdr lst))))))
	 (add-part  (lambda (str start end out)
		      ;; TODO: don't allocate new substrings
		      ;; for each call to string-append; need
		      ;; something like "substring-append"
                      (if (= (string-length out) 0)
                        (##sys#substring str start end)
                        (cond
                          ((or (= start end) (seg=? str start end "."))
                           out)
                          ((seg=? str start end "..")
                           (if (string-suffix? ".." out)
                             (string-append out "/..")
                             (dirname out)))
                          ((string=? out "/")
			   (string-append out (##sys#substring str start end)))
                          (else (string-append out "/" (##sys#substring str start end)))))))
	 (add-arg   (lambda (arg out)
		      (strsep add-part (stringify arg) #\/ out))))
    (lambda (first . rest)
      (fold
       add-arg
       ;; if the first component begins with "/", then
       ;; the result begins with "/"
       (if (eqv? (string-ref (stringify first) 0) #\/)
         "/"
         "")
       (cons first rest)))))

;; convert a relative path to an absolute path
;; if it is not one already (by prepending the current directory)
(: abspath (string --> string))
(define (abspath p)
  (if (eq? (string-ref p 0) #\/) p (filepath-join (current-directory) p)))
