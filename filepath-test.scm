(include "sequence.mod.scm")
(include "filepath.mod.scm")

(import
  (only (chicken process-context) current-directory)
  (distill filepath))

(include "test-helpers.scm")

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

