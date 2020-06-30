(foreign-declare "#include <string.h>")
(foreign-declare "#include <dirent.h>")
(foreign-declare "
static int dsort(const struct dirent **a, const struct dirent **b) {
  return strcmp((*a)->d_name, (*b)->d_name);
}
")

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

;; folddir folds (proc file seed) over
;; every file under the directory 'path'
;; in strcmp-sorted order (but omitting
;; the "." and ".." entries)
;;
;; note that this is not a recursive directory traversal
(: folddir ((string 'a -> 'a) 'a string -> 'a))
(define (folddir proc seed path)
  (let-location ((namelist (c-pointer (c-pointer (struct dirent)))))
    (let* ((scandir (foreign-lambda* int ((nonnull-c-string dirp) (nonnull-c-pointer nl))
		      "C_return(scandir(dirp,nl,NULL,dsort));"))
	   (entname (foreign-lambda* c-string ((nonnull-c-pointer dirents) (int i))
		      "C_return(((struct dirent **)dirents)[i]->d_name);"))
	   (freeidx (foreign-lambda* void ((nonnull-c-pointer dirent) (int i))
		      "free(((struct dirent **)dirent)[i]); ((struct dirent **)dirent)[i]=NULL;"))
	   (free    (foreign-lambda* void ((nonnull-c-pointer dirents))
		      "free(dirents);"))
	   (entries (scandir path (location namelist))))
      (if (fx< entries 0)
	  (error "cannot scandir(3) directory" path)
	  (let loop ((i 0)
		     (out seed))
	    (if (fx>= i entries)
		(begin
		  (free namelist)
		  out)
		;; chicken will automatically copy out
		;; the c-string contents here, so we
		;; are safe to free the dirent as soon
		;; as we pull its name out into a
		;; scheme string
		(let* ((str (entname namelist i))
		       (_   (freeidx namelist i)))
		  (loop
		      (fx+ i 1)
		      (if (or (string=? str ".")
			      (string=? str ".."))
			  out
			  (proc str out))))))))))
