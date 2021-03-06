
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

(defun when-let-expander (args body)
  (let* ((grouped-args (group args 2))
         (arg-vars (mapcar #'first grouped-args))
         (arg-values (mapcar #'second grouped-args))
         (arg-var-names (mapcar #'second arg-vars)))
    `(:CALL
      (:FUNCTION NIL ,arg-var-names
                 ((:IF ,(make-binary-and-ast arg-vars)
                       (:BLOCK
                           ,body)
                       NIL)))
      ,arg-values)))

(defun group (list n)
  (if (< (length list) n)
      (if list
          (list list))
      (cons (subseq list 0 n)
            (group (subseq list n) n))))

(defun make-binary-and-ast (operands)
  (cond ((= 1 (length operands))
         (first operands))
        ((= 2 (length operands))
         (list* :binary :&& operands))
        ((> (length operands) 2)
         (let ((first-two (subseq operands 0 2))
               (rest (subseq operands 2)))
           (make-binary-and-ast (cons (make-binary-and-ast first-two)
                                      rest))))
        (t (error "Incorrect number of operands for @whenLet."))))

(test when-let
  (cheat-js:clear-macros)
  (cheat-js:register-args-and-body-macro "@whenLet")
  (let ((js-code "@whenLet(t1, 1, t2, 2, t3, 3; f(t1, t2, t3););"))
    (is (equal (cheat-js:parse-js js-code)
               '(:TOPLEVEL
                 ((:STAT
                   (:MACRO-CALL (:NAME "@whenLet")
                                ((:ARGS (:NAME "t1") (:NUM 1)
                                        (:NAME "t2") (:NUM 2)
                                        (:NAME "t3") (:NUM 3))
                                 (:BODY
                                  (:STAT (:CALL (:NAME "f") ((:NAME "t1")
                                                             (:NAME "t2")
                                                             (:NAME "t3"))))))))))))
    (cheat-js:register-macro-expander "@whenLet" #'when-let-expander)
    (is (equal (cheat-js:explode "@whenLet(t1, 1, t2, 2, t3, 3; f(t1, t2, t3););")
               "(function(t1, t2, t3) {
    if (t1 && t2 && t3) {
        f(t1, t2, t3);
    }
})(1, 2, 3);"))))

(defun awhen-expander (args body)
  `(:MACRO-CALL (:NAME "@whenLet")
                ((:ARGS (:NAME "it") ,(first args))
                 (:BODY ,@body))))

(test awhen
  (cheat-js:clear-macros)
  (cheat-js:register-args-and-body-macro "@whenLet")
  (cheat-js:register-macro-expander "@whenLet" #'when-let-expander)
  (cheat-js:register-args-and-body-macro "@awhen")
  (cheat-js:register-macro-expander "@awhen" #'awhen-expander)
  (is (equal (cheat-js:explode "@awhen(expr;f(expr););")
             "(function(it) {
    if (it) {
        f(expr);
    }
})(expr);")))

(defun init-library-tests ()
  (cheat-js:clear-macros)
  (cj-macro-library:install-macros (cj-macro-library:list-macros))
  (cj-macro-library:reset-gensym-counter))

(defun one-library-test (invocation expansion)
  (is (equal (cheat-js:explode invocation)
             expansion)))

(test library-iife-1
  (init-library-tests)
  (one-library-test "var Test = @iife(return 2;);"
                    "var Test = function() {
    return 2;
}();"))

(test library-iife-2
  (init-library-tests)
  (one-library-test "@iife(doThis(); thenDoThat(););"
                    "(function() {
    doThis();
    thenDoThat();
})();"))

(test library-defclass-1
  (init-library-tests)
  (one-library-test "var Person = @defclass(Person, name, shoeSize);"
                    "var Person = function() {
    function _Person(name, shoeSize) {
        this.name = name;
        this.shoeSize = shoeSize;
    }
    function Person(name, shoeSize) {
        var self = new _Person(name, shoeSize);
        return self;
    }
    return Person;
}();"))

(test library-defclass-2
  (init-library-tests)
  (one-library-test "var Person = @defclass(
    Person, name, shoeSize;
    self.firstName = name.split(' ')[0];
);"
                    "var Person = function() {
    function _Person(name, shoeSize) {
        this.name = name;
        this.shoeSize = shoeSize;
    }
    function Person(name, shoeSize) {
        var self = new _Person(name, shoeSize);
        self.firstName = name.split(\" \")[0];
        return self;
    }
    return Person;
}();"))

(test library-defclass-3
  (init-library-tests)
  (one-library-test "var Person = @defclass(
    Person, name, shoeSize, $that;
    that.firstName = name.split(' ')[0];
);"
                    "var Person = function() {
    function _Person(name, shoeSize) {
        this.name = name;
        this.shoeSize = shoeSize;
    }
    function Person(name, shoeSize) {
        var that = new _Person(name, shoeSize);
        that.firstName = name.split(\" \")[0];
        return that;
    }
    return Person;
}();"))

(test library-if
  (init-library-tests)
  (one-library-test "var a = @if(someCondition,thenResult,elseResult);"
                    "var a = someCondition ? thenResult : elseResult;"))

(test library-and-or
  (init-library-tests)
  (one-library-test "if (@and(cond1,cond2,cond3,@or(cond4,cond5))) { doIt(); }"
                    "if (cond1 && cond2 && cond3 && (cond4 || cond5)) {
    doIt();
}"))

(test library-fn
  (init-library-tests)
  (one-library-test "@fn(a, b; a < b)"
                    "function(a, b) {
    return a < b;
};"))

(test library-fn0
  (init-library-tests)
  (one-library-test "var action = @fn0(
    doThis();
    thenDoThat();
);"
                    "var action = function() {
    doThis();
    thenDoThat();
};"))

(test library-dbind-1
  (init-library-tests)
  (one-library-test "@dbind([a, b], [1, 2]);"
                    "{
    var tmp1 = [ 1, 2 ], a = tmp1[0], b = tmp1[1];
};"))

(test library-dbind-2
  (init-library-tests)
  (one-library-test "@dbind([a, b, c], [1, 2, 'a']);"
                    "{
    var tmp1 = [ 1, 2, \"a\" ], a = tmp1[0], b = tmp1[1], c = tmp1[2];
};"))

(test library-dotimes-1
  (init-library-tests)
  (one-library-test "@dotimes(a+b; doit());"
                    "{
    var limit2 = a + b;
    for (var i1 = 0; i1 < limit2; i1++) {
        doit();
    }
};"))

(test library-dotimes-2
  (init-library-tests)
  (one-library-test "@dotimes(j, a+b; doit());"
                    "{
    var limit1 = a + b;
    for (var j = 0; j < limit1; j++) {
        doit();
    }
};"))

(test library-dolist-1
  (init-library-tests)
  (one-library-test "@dolist(j, $i, a(b); doIt(i, j); doItAgain(j););"
                    "{
    var list1 = a(b), len2 = list1.length;
    for (var i = 0; i < len2; i++) {
        var j = list1[i];
        doIt(i, j);
        doItAgain(j);
    }
};"))

(test library-dolist-2
  (init-library-tests)
  (one-library-test "@dolist(j, a(b), $i; doIt(i, j); doItAgain(j););"
                    "{
    var list1 = a(b), len2 = list1.length;
    for (var i = 0; i < len2; i++) {
        var j = list1[i];
        doIt(i, j);
        doItAgain(j);
    }
};"))

(test library-dolist-3
  (init-library-tests)
  (one-library-test "@dolist(j, a(b); doIt(j); doItAgain(j););"
                    "{
    var list2 = a(b), len3 = list2.length;
    for (var i1 = 0; i1 < len3; i1++) {
        var j = list2[i1];
        doIt(j);
        doItAgain(j);
    }
};"))
