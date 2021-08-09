(import
 scheme
 (only (chicken string) string-translate* string-intersperse string-split)
 (distill package)
 (distill base))

(define libexpat
  (let* ((ver "2.4.1")
         (tag (string-intersperse
               (cons "R" (string-split ver ".")) "_"))
         (url (string-translate*
               "https://github.com/libexpat/libexpat/releases/download/$tag/$name-$version.tar.gz"
               `(("$tag" . ,tag)))))
    (cmmi-package
     "expat" ver url
     "aYNm2Avpk6T5Kqfpi5tSc7duEAokmSD-YEgN1_inR1w="
     extra-configure: '(--without-examples
                        --without-tests
                        --with-getrandom
                        --without-docbook))))
