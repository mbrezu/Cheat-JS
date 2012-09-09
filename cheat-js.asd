;;;; cheat-js.asd

(asdf:defsystem #:cheat-js
  :serial t
  :description "Macros for JavaScript. Kinda."
  :author "Miron Brezuleanu"
  :license "Simplified BSD License"
  :depends-on (#:fiveam
               #:cl-uglify-js)
  :components ((:file "package")
               (:file "util" :depends-on ("package"))
               (:file "tokenize" :depends-on ("package" "util"))
               (:file "parse" :depends-on ("package" "util" "tokenize"))
               (:file "cheat-js" :depends-on ("parse"))
               (:file "tests" :depends-on ("cheat-js"))))
