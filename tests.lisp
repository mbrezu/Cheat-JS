
(in-package :cheat-js-tests)

(def-suite :cheat-js-tests :description "Tests for CSS generation.")

(in-suite :cheat-js-tests)

(defun defclass-expander (invocation)
  (let* ((raw-names (cdr (second (third invocation))))
         (names (mapcar #'second raw-names)))
    `(:function nil ,names
                ,(mapcar (lambda (name)
                           `(:stat
                             (:assign t
                                      (:dot (:name "this") ,name)
                                      (:name ,name))))
                         names))))

(test defclass
  (cheat-js:clear-macros)
  (cheat-js:register-args-macro "@defclass")
  (let ((js-code "var Person = @defclass(name, shoeSize);"))
    (is (equal (cheat-js:parse-js js-code)
               '(:TOPLEVEL
                 ((:VAR
                   (("Person" :MACRO-CALL (:NAME "@defclass")
                              (:ARGS (:SEQ (:NAME "name")
                                           (:NAME "shoeSize"))))))))))
    (cheat-js:register-macro-expander "@defclass" #'defclass-expander)
    (is (equal (cheat-js:explode js-code)
               "var Person = function(name, shoeSize) {
    this.name = name;
    this.shoeSize = shoeSize;
};"))))
