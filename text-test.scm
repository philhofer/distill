(import
 scheme
 (chicken string)
 (distill text))

(include "test-helpers.scm")

(test* string=? join-with
       ((""      "," '())
        ("a,b"   "," (list "a" "b"))
        ("a"     "," (list "a"))
        ("a,b,c" "," (list "a,b" "c"))))

(test* string=? tabular
       (("first line\nsecond line\n" identity " " "\n" '((first line) (second line)))
        ("a,b,c\ne,f,g\n"            identity "," "\n" '((a b c) (e f g)))
        ("a\nc\n"                    car      " " "\n" '((a b) (c d)))))

(test* string=? lines
       (("hello\nworld\n"
         '(hello world))
        ("first line contents\nsecond line contents\n"
         '((first line contents) (second line contents)))))

(test string=?
      "first line\nsecond line\n"
      (map-lines (cut string-split <> "|") '("first|line" "second|line")))
