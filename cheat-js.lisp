;;;; cheat-js.lisp

(in-package #:cheat-js)

;;; "cheat-js" goes here. Hacks and glory await!

(defvar *macros* (make-hash-table :test #'equal))

(defun clear-macros ()
  (setf *macros* (make-hash-table :test #'equal)))

(defstruct macro-record name kind expander)

(defun register-macro (macro-name macro-kind macro-expander)
  (cond ((null (gethash macro-name *macros*))
         (setf (gethash macro-name *macros*)
               (make-macro-record :name macro-name
                                  :kind macro-kind
                                  :expander macro-expander)))
        (t (setf (macro-record-kind (gethash macro-name *macros*))
                 macro-kind)
           (setf (macro-record-expander (gethash macro-name *macros*))
                 macro-expander))))

(defun register-args-macro (macro-name)
  (register-macro macro-name :args nil))

(defun register-body-macro (macro-name)
  (register-macro macro-name :body nil))

(defun register-args-and-body-macro (macro-name)
  (register-macro macro-name :args-and-body nil))

(defun register-macro-expander (macro-name macro-expander)
  (let ((macro-record (gethash macro-name *macros*)))
    (unless macro-record
      (error (format nil "Cheat-JS: Cannot find macro \"~a\"." macro-name)))
    (register-macro macro-name
                    (macro-record-kind macro-record)
                    macro-expander)))

(defun macro-hook (name)
  (or (let ((macro (gethash name *macros*)))
        (if macro
            (macro-record-kind macro)))
      :not-a-macro))

(defun parse-js (js-string)
  (let ((parse-js:*macro-hook* #'macro-hook)
        (parse-js:*allow-at-signs* t))
    (parse-js:parse-js js-string)))

(defun expand-macro (macro-record ast)
  (ecase (macro-record-kind macro-record)
    ((:args) (funcall (macro-record-expander macro-record) (rest (third ast))))
    ((:body) (funcall (macro-record-expander macro-record) (rest (third ast))))
    ((:args-and-body)
     (funcall (macro-record-expander macro-record)
              (rest (first (third ast)))
              (rest (second (third ast)))))))

(defun macroexpand-all (ast)
  (cond ((listp ast)
         (cond ((eq :var (first ast))
                (list :var
                      (mapcar (lambda (pair)
                                (destructuring-bind (var-name &rest ast)
                                    pair
                                  (cons var-name (macroexpand-all ast))))
                              (second ast))))
               ((eq :macro-call (first ast))
                (let ((macro-name (second (second ast))))
                  (let ((macro-record (gethash macro-name *macros*)))
                    (unless macro-record
                      (error (format nil "Cheat-JS: unknown macro \"~a\"."
                                     macro-name)))
                    (unless (macro-record-expander macro-record)
                      (error (format
                              nil
                              "Cheat-JS: no expander defined for macro \"~a\"."
                              macro-name)))
                    (let ((expansion (expand-macro macro-record ast)))
                      (macroexpand-all expansion)))))
               (t (mapcar #'macroexpand-all ast))))
        (t ast)))

(defun explode (js-string)
  (cl-uglify-js:ast-gen-code (macroexpand-all (parse-js js-string))))

(defun run-tests ()
  (fiveam:run! :cheat-js-tests))
