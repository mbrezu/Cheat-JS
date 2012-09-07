;;;; package.lisp

(cl:defpackage #:parse-js
  (:use #:cl)
  (:export #:token-type #:token-value #:token-line #:token-char #:token-pos
           #:token-newline-before #:token-comments-before
           #:lex-js #:parse-js #:parse-js-string #:read-js-number
           #:js-parse-error #:js-parse-error-line #:js-parse-error-char
           #:*check-for-reserved-words* #:*ecma-version* 
           #:*allow-at-signs*
           #:*macro-hook*))

(defpackage #:cheat-js
  (:use #:cl))

