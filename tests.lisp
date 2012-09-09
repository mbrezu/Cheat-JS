
(in-package :cheat-js-tests)

(def-suite :cheat-js-tests :description "Tests for CSS generation.")

(in-suite :cheat-js-tests)

(defun defclass-expander (args)
  (let* ((names (mapcar #'second args)))
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
                              (:ARGS (:NAME "name") (:NAME "shoeSize")))))))))
    (cheat-js:register-macro-expander "@defclass" #'defclass-expander)
    (is (equal (cheat-js:explode js-code)
               "var Person = function(name, shoeSize) {
    this.name = name;
    this.shoeSize = shoeSize;
};"))))

(defun iife-expander (body)
  `(:CALL (:FUNCTION NIL NIL ,body)
          NIL))

(test iife
  (cheat-js:clear-macros)
  (cheat-js:register-body-macro "@iife")
  (let ((js-code "var a = @iife(alert(1);alert(2););"))
    (is (equal (cheat-js:parse-js js-code)
               '(:TOPLEVEL
                 ((:VAR
                   (("a" :MACRO-CALL (:NAME "@iife")
                         (:BODY (:STAT (:CALL (:NAME "alert") ((:NUM 1))))
                                (:STAT (:CALL (:NAME "alert") ((:NUM 2))))))))))))
    (cheat-js:register-macro-expander "@iife" #'iife-expander)
    (is (equal (cheat-js:explode js-code)
               "var a = function() {
    alert(1);
    alert(2);
}();"))
    (is (equal (cheat-js:explode "@iife(alert(1);alert(2););")
               "(function() {
    alert(1);
    alert(2);
})();"))))

(defun safe-defclass-expander (args)
  (let* ((names (mapcar #'second args))
         (class-name (first names))
         (field-names (rest names)))
    `(:MACRO-CALL
      (:NAME "@iife")
      (:BODY
       (:DEFUN ,class-name ,field-names
         ,(mapcar (lambda (field)
                    `(:STAT (:ASSIGN T
                                     (:DOT (:NAME "this") ,field)
                                     (:NAME ,field))))
                  field-names))
       (:BLOCK NIL)
       (:RETURN
         (:FUNCTION NIL ,field-names
                    ((:RETURN (:NEW (:NAME ,class-name)
                                    ,(mapcar (lambda (field)
                                               (list :name field))
                                             field-names))))))))))
(test safe-defclass
  (cheat-js:clear-macros)
  (cheat-js:register-body-macro "@iife")
  (cheat-js:register-macro-expander "@iife" #'iife-expander)
  (cheat-js:register-args-macro "@safeDefclass")
  (let ((js-code "var Person = @safeDefclass(Person, name, shoeSize);"))
    (is (equal (cheat-js:parse-js js-code)
               '(:TOPLEVEL
                 ((:VAR
                   (("Person" :MACRO-CALL (:NAME "@safeDefclass")
                              (:ARGS (:NAME "Person")
                                     (:NAME "name")
                                     (:NAME "shoeSize")))))))))
    (cheat-js:register-macro-expander "@safeDefclass"
                                      #'safe-defclass-expander)
    (is (equal (cheat-js:explode js-code)
               "var Person = function() {
    function Person(name, shoeSize) {
        this.name = name;
        this.shoeSize = shoeSize;
    }
    return function(name, shoeSize) {
        return new Person(name, shoeSize);
    };
}();"))))
