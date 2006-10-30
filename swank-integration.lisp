;; -*- mode: Lisp; Syntax: Common-Lisp; Package: computed-class; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a copy 
;;; of this software and associated documentation files (the "Software"), to deal 
;;; in the Software without restriction, including without limitation the rights 
;;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell 
;;; copies of the Software, and to permit persons to whom the Software is furnished 
;;; to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in 
;;; all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR 
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE 
;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER 
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING 
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS 
;;; IN THE SOFTWARE.

(in-package :computed-class)

(enable-sharp-boolean-syntax)

(in-package :swank)

;; when inspecting a computed slot, display the computed-state
#+#.(cl:when (cl:find-package "SWANK") '(:and))
(defmethod inspect-for-emacs ((computed-class::object computed-class::computed-object) inspector)
  (declare (ignore inspector))
  (let ((c (class-of computed-class::object)))
    (values "An object."
            `("Class: " (:value ,c) (:newline)
              "Slots:" (:newline)
              ,@(loop for computed-class::slot in (swank-mop:class-slots c)
                      for name = (swank-mop:slot-definition-name computed-class::slot)
                      collect `(:value ,computed-class::slot ,(string name))
                      collect " = "
                      collect (if (swank-mop:slot-boundp-using-class c computed-class::object computed-class::slot)
                                  `(:value ,(if (typep computed-class::slot 'computed-class::computed-effective-slot-definition)
                                                #.(computed-class::standard-instance-access-form)
                                                (swank-mop:slot-value-using-class 
                                                 c computed-class::object computed-class::slot)))
                                  "#<unbound>")
                      collect '(:newline))))))

