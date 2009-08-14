;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.computed-class)

(hu.dwim.syntax-sugar:register-readtable-for-swank
 '(:hu.dwim.computed-class :hu.dwim.computed-class-test) 'setup-readtable)

;; when inspecting a computed slot, display the computed-state
(defmethod swank:inspect-slot-for-emacs ((class computed-class)
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
