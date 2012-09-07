
;; Copyright (c) Marijn Haverbeke, marijnh@gmail.com

;; This software is provided 'as-is', without any express or implied
;; warranty. In no event will the authors be held liable for any
;; damages arising from the use of this software.

;; Permission is granted to anyone to use this software for any
;; purpose, including commercial applications, and to alter it and
;; redistribute it freely, subject to the following restrictions:

;; 1. The origin of this software must not be misrepresented; you must
;;    not claim that you wrote the original software. If you use this
;;    software in a product, an acknowledgment in the product
;;    documentation would be appreciated but is not required.

;; 2. Altered source versions must be plainly marked as such, and must
;;    not be misrepresented as being the original software.

;; 3. This notice may not be removed or altered from any source
;;    distribution.

;; ## Important Note

;; Cheat-JS includes a modified version of
;; [`parse-js`](http://marijnhaverbeke.nl/parse-js/), written by Marijn
;; Haverbeke. This is necessary because I (Miron Brezuleanu) needed to
;; modify `parse-js` a little. The license of `parse-js` is in the
;; `LICENSE-parse-js.txt` file. The modified files from `parse-js`
;; included in Cheat-JS are `parse.lisp`, `tokenize.lisp` and
;; `util.lisp`. The modifications were permitted by the `parse-js`
;; license. This is not an official copy of `parse-js` and is not
;; supported by Marijn Haverbeke. If the modified parsing code in
;; Cheat-JS breaks, it's exclusively my fault - I messed up the code.

(in-package #:parse-js)

(defmacro with-defs (&body body)
  (loop :for form :in body
     :if (and (eq (car form) 'def) (< (length form) 4))
     :collect (cadr form) :into vars :and
     :if (caddr form) :collect `(setf ,(cadr form) ,(caddr form)) :into body :end
     :else :if (eq (car form) 'def)
     :collect (cdr form) :into funcs
     :else
     :collect form :into body
     :finally (return `(let ,vars (labels ,funcs ,@body)))))

(defmacro defun/defs (name args &body body)
  `(defun ,name ,args (with-defs ,@body)))
