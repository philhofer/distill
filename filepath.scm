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
        ((char=? (string-ref str i) #\/) (substring/shared str 0 i))
        (else (loop (- i 1)))))))

(: basename (string --> string))
(define (basename str)
  (let ((end (- (string-length str) 1)))
    (let loop ((i end))
      (cond
        ((< i 0) str)
        ((char=? (string-ref str i) #\/) (substring/shared str (+ i 1)))
        (else (loop (- i 1)))))))

(define (suffixed? str suf)
  (and-let* ((slen   (string-length str))
             (suflen (string-length suf))
             (_      (>= slen suflen))
             (tail   (substring/shared str (- slen suflen) slen)))
    (string=? suf tail)))

;; core filepath normalization routine
;;
;; join one or more filepath components together
;; while eliminating '.' and '..' components where possible
(: filepath-join (stringy #!rest stringy --> string))
(define filepath-join
  ;; allocate the inner reducing function closure just once
  ;; (these produce a single-pass reducer from path components
  ;; to output path string)
  (let* ((add-part  (lambda (part out)
                      (if (string=? out "")
                        part
                        (cond
                          ((or (string=? part "") (string=? part "."))
                           out)
                          ((string=? part "..")
                           (if (suffixed? out "..")
                             (string-append out "/..")
                             (dirname out)))
                          ((string=? out "/") (string-append out part))
                          (else (string-append out "/" part))))))
         (cons-arg  (k/map stringify (k/map (cut string-sep->seq <> #\/) (k/recur add-part)))))
    (lambda (first . rest)
      ((list->seq (cons first rest))
       cons-arg
       ;; if the first component begins with "/", then
       ;; the result begins with "/"
       (if (eqv? (string-ref (stringify first) 0) #\/)
         "/"
         "")))))

;; convert a relative path to an absolute path
;; if it is not one already (by prepending the current directory)
(: abspath (string --> string))
(define (abspath p)
  (if (eq? (string-ref p 0) #\/) p (filepath-join (current-directory) p)))
