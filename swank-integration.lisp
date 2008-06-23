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

#+#.(cl:when (cl:find-package "SWANK") '(:and))
(eval-always
  (use-package :swank))

#+#.(cl:when (cl:find-package "SWANK") '(:and))
(progn

(unless (assoc "COMPUTED-CLASS" *readtable-alist* :test #'string=)
  (let ((*readtable* (copy-readtable)))
    (setup-readtable)
    (push (cons "COMPUTED-CLASS" *readtable*) *readtable-alist*)
    (push (cons "COMPUTED-CLASS-TEST" *readtable*) *readtable-alist*)))

;; when inspecting a computed slot, display the computed-state
(defmethod inspect-slot-for-emacs ((class computed-class)
                                   (object computed-object)
                                   (slot computed-effective-slot-definition))
  ;; we skip svuc to avoid recalculation of invalid slots
  (let ((value (standard-instance-access-form object slot)))
    (cond ((eq value '#.+unbound-slot-value+)
           '("#<unbound>"))
          ((computed-state-p value)
           `(,(if (computed-state-valid-p value) "Valid: " "Invalid: ")
             (:value ,(cs-value value))
             ,(concatenate 'string ", pulse: " (princ-to-string (cs-computed-at-pulse value)) "/" (princ-to-string (cs-validated-at-pulse value)))
             ", " (:value ,value ,(cu-name (cs-universe value)))
             " "
             ,(if (computed-state-valid-p value)
                  `(:action "[invalidate]" ,(lambda () (invalidate-computed-state value)))
                  `(:action "[compute]" ,(lambda () (ensure-computed-state-is-valid value))))
             " "
             (:action "[make unbound]" ,(lambda () (slot-makunbound-using-class class object slot)))))
          (t (call-next-method)))))

)

