
(in-package :cj-macro-library)

(defvar *dependencies*)

(setf *dependencies* '(("@defclass" "@iife")))

(defun iife-expander (body)
  `(:CALL (:FUNCTION NIL NIL ,body)
          NIL))

(defun defclass-expander (args body)
  (labels ((starts-with-$ (name)
             (char= (elt name 0) #\$)))
    (let* ((names (mapcar #'second args))
           (filtered-names (remove-if #'starts-with-$ names))
           (candidate-self-name (first (remove-if (complement #'starts-with-$)
                                                  names)))
           (self-name (or (if candidate-self-name (subseq candidate-self-name 1))
                          "self"))
           (class-name (first filtered-names))
           (inner-class-name (format nil "_~a" class-name))
           (field-names (rest filtered-names)))
      `(:MACRO-CALL
        (:NAME "@iife")
        (:BODY
         (:DEFUN ,inner-class-name ,field-names
           ,(mapcar (lambda (field)
                      `(:STAT (:ASSIGN T
                                       (:DOT (:NAME "this") ,field)
                                       (:NAME ,field))))
                    field-names))
         (:DEFUN ,class-name ,field-names
           ((:var ((,self-name :NEW (:NAME ,inner-class-name)
                               ,(mapcar (lambda (field)
                                          (list :name field))
                                        field-names))))
            ,@body
            (:RETURN (:name ,self-name))))
         (:RETURN (:name ,class-name)))))))

(defun if-expander (args)
  `(:conditional ,@args))

(defun make-binary-ast (operator operands)
  (cond ((= 1 (length operands))
         (first operands))
        ((= 2 (length operands))
         (list* :binary operator operands))
        ((> (length operands) 2)
         (let ((first-two (subseq operands 0 2))
               (rest (subseq operands 2)))
           (make-binary-ast operator
                            (cons (make-binary-ast operator first-two)
                                  rest))))
        (t (error "Incorrect number of operands for @whenLet."))))

(defun and-expander (args)
  (make-binary-ast ':&& args))

(defun or-expander (args)
  (make-binary-ast ':\|\| args))

(defun fn-expander (args body)
  (unless (= 1 (length body))
    (error "@fn macro: body must have only one expression."))
  `(:FUNCTION NIL ,(mapcar #'second args)
              ((:return ,(second (first body))))))

(defun fn0-expander (body)
  `(:FUNCTION NIL nil ,body))

(defvar *cj-gensym-counter* 0)

(defun cj-gensym (&optional (prefix "tmp"))
  (format nil "~a~d" prefix (incf *cj-gensym-counter*)))

(defun dbind-expander (args)
  (let* ((vars (second (first args)))
         (indexes (loop for i from 0 to (1- (length vars)) collect i))
         (val (second args))
         (tmp (cj-gensym)))
    `(:BLOCK
         ((:VAR
           ((,tmp ,@val)
            ,@(mapcar (lambda (var idx)
                        `(,(second var) :sub (:name ,tmp) (:num ,idx)))
                      vars
                      indexes)))))))

(defun dotimes-expander (args body)
  (let ((var (if (= 2 (length args))
                 (second (first args))
                 (cj-gensym "i")))
        (limit (if (= 2 (length args))
                   (second args)
                   (first args)))
        (limit-gensym (cj-gensym "limit")))
    `(:BLOCK
         ((:VAR ((,limit-gensym ,@limit)))
          (:FOR (:VAR ((,var :NUM 0)))
                (:BINARY :< (:NAME ,var) (:NAME ,limit-gensym))
                (:UNARY-POSTFIX :++ (:NAME ,var))
                (:BLOCK ,body))))))

(defun dolist-expander (args body)
  (labels ((starts-with-$ (name)
             (and (listp name)
                  (eq :name (first name))
                  (char= (elt (second name) 0) #\$))))
    (let* ((filtered-args (remove-if #'starts-with-$ args))
           (iterator-variable-candidate (second
                                         (first
                                          (remove-if (complement #'starts-with-$)
                                                     args))))
           (iterator-var (or (if iterator-variable-candidate
                                 (subseq iterator-variable-candidate 1))
                             (cj-gensym "i")))
           (list-gensym (cj-gensym "list"))
           (len-gensym (cj-gensym "len"))
           (item-variable (first filtered-args))
           (list-expression (second filtered-args)))
      `(:BLOCK
           ((:VAR
             ((,list-gensym ,@list-expression)
              (,len-gensym :DOT (:NAME ,list-gensym) "length")))
            (:FOR (:VAR ((,iterator-var :NUM 0)))
                  (:BINARY :< (:NAME ,iterator-var) (:NAME ,len-gensym))
                  (:UNARY-POSTFIX :++ (:NAME ,iterator-var))
                  (:BLOCK
                      ((:VAR ((,(second item-variable) :SUB (:NAME ,list-gensym)
                                (:NAME ,iterator-var))))
                       ,@body))))))))

(defvar *macros*)

(setf *macros* (list (list "@iife" :body #'iife-expander)
                     (list "@defclass" :args-and-body #'defclass-expander)
                     (list "@if" :args #'if-expander)
                     (list "@and" :args #'and-expander)
                     (list "@or" :args #'or-expander)
                     (list "@fn" :args-and-body #'fn-expander)
                     (list "@fn0" :body #'fn0-expander)
                     (list "@dbind" :args #'dbind-expander)
                     (list "@dotimes" :args-and-body #'dotimes-expander)
                     (list "@dolist" :args-and-body #'dolist-expander)))

(defun reset-gensym-counter ()
  (setf *cj-gensym-counter* 0))

(defun list-macros ()
  (mapcar #'first *macros*))

(defun install-macros (macros)
  (let (all-macros)
    (labels ((add-with-dependencies-recursively (macro)
               (pushnew macro all-macros)
               (dolist (dependency (cdr (assoc macro *dependencies* :test #'equal)))
                 (add-with-dependencies-recursively dependency))))
      (dolist (macro macros)
        (add-with-dependencies-recursively macro)))
    (dolist (macro all-macros)
      (let ((macro-data (cdr (assoc macro *macros* :test #'equal))))
        (unless macro-data
          (error "Macro '~a' not found." macro))
        (cheat-js:register-macro macro (first macro-data) (second macro-data))))
    all-macros))
