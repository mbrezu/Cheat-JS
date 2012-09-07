;;;; cheat-js.asd

(asdf:defsystem #:cheat-js
  :serial t
  :description "TBD: Describe cheat-js here"
  :author "Miron Brezuleanu"
  :license "TBD: Specify license here"
  :depends-on (#:fiveam
               #:cl-uglify-js)
  :components ((:file "package")
               (:file "util" :depends-on ("package"))
               (:file "tokenize" :depends-on ("package" "util"))
               (:file "parse" :depends-on ("package" "util" "tokenize"))
               (:file "cheat-js" :depends-on ("parse"))
               (:file "tests" :depends-on ("cheat-js"))))
