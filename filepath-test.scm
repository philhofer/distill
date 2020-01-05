(cond-expand
  (csi (import r7rs)
       (load "filepath.sld"))
  (else (begin)))

(import
  (only (chicken process-context) current-directory)
  (filepath))

(include "test-helpers.scm")

(for-each
  (lambda (p)
    (let ((want (car p))
	  (got  (let ((left (cdr p)))
		  (if (pair? left)
		      (apply filepath-join left)
		      (filepath-join left)))))
      (test string=? want got)))
  '(("a/b/c" . "a/b/c")
    ("a/b/c" . "a/b/c/")
    ("a/b/c" "a" "b" "c")
    ("/a/b/c" . "/a/b/c")
    ("/b/c" "/a/" "../b" "c")
    ("/a/b/c" "/a" "./b/" "./c")
    ("a/b/c/d" "a/b" "c/d")
    ("foo/bar" "foo" "baz" ".." "bar")))

(test string=? (current-directory) (abspath "."))
(test string=? (filepath-join (current-directory) "foo") (abspath "./foo"))

(test string=? "/foo" (dirname "/foo/bar.txt"))
(test string=? "/foo/bar/baz" (dirname "/foo/bar/baz/"))
(test string=? "/foo/bar" (dirname "/foo/bar/baz.txt"))
(test string=? "." (dirname "foo"))
(test string=? "/" (dirname "/"))
(test string=? "/" (dirname "/foo"))

(test string=? "foo" (basename "/foo"))
(test string=? "bar.txt" (basename "/foo/bar.txt"))
(test string=? "foo" (basename "foo"))
(test string=? "." (basename "."))
(test string=? "" (basename "/"))
(test string=? "" (basename "/foo/bar/"))
