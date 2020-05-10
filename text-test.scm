(import
  scheme
  (chicken string)
  (distill text))

(include "test-helpers.scm")

(test* string=? join-with ((""      "," '())
			   ("a,b"   "," (list "a" "b"))
			   ("a"     "," (list "a"))
			   ("a,b,c" "," (list "a,b" "c"))))

(test string=?
      "hello\nworld\n"
      (lines '(hello world)))

(test string=?
      "first line contents\nsecond line contents\n"
      (lines '((first line contents)
	       (second line contents))))
(test string=?
      "first line\nsecond line\n"
      (map-lines (cut string-split <> "|") '("first|line" "second|line")))
