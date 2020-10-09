(import
 (only (chicken process-context) current-directory)
 (distill filepath))

(include "test-helpers.scm")

(test* eq? string-prefix?
       ((#t "pre" "prefix")
        (#t "pre" "pre")
        (#f "pre" "pr")
        (#t ""    "anything")
        (#f "anything" "")))

(test* eq? string-suffix?
       ((#t "fix" "prefix")
        (#f "ref" "prefix")
        (#t "fix" "fix")
        (#t ""    "anything")
        (#f "anything" "")))

(test* string=? filepath-join
       (("a/b/c"   "a/b/c")
        ("a/b/c"   "a/b/c/")
        ("a/b/c"   "a" "b" "c")
        ("/a/b/c"  "/a/b/c")
        ("/b/c"    "/a/" "../b" "./c")
        ("/a/b/c"  "/a" "./b/" "./c")
        ("a/b/c/d" "a/b" "c/d")
        ("foo/bar" "foo" "baz" ".." "bar")))

(test string=? (current-directory) (abspath "."))
(test string=? (filepath-join (current-directory) "foo") (abspath "./foo"))

(test* string=? dirname
       (("/foo"         "/foo/bar.txt")
        ("/foo/bar/baz" "/foo/bar/baz/")
        ("/foo/bar"     "/foo/bar/baz.txt")
        ("."            "foo")
        ("/"            "/")
        ("/"            "/foo")))

(test* string=? basename
       (("foo"     "/foo")
        ("bar.txt" "/foo/bar.txt")
        ("foo"     "foo")
        ("."       ".")
        (""        "/")
        (""        "/foo/bar/")))

(let ((rdir (folddir cons '() (current-directory))))
  (test eq? #t
        (let loop ((head (car rdir))
                   (rest (cdr rdir)))
          (or (null? rest)
              (and
               (string<? (car rest) head)
               (loop (car rest) (cdr rest)))))))
