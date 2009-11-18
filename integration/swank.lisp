;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.computed-class)

;; when inspecting a computed slot with a computed state inside it, then display the computed-state details
(defmethod swank::slot-value-for-inspector ((class computed-class)
                                            (object computed-object)
                                            (slot computed-effective-slot-definition))
  ;; we skip svuc to avoid recalculation of invalid slots
  (let ((value (standard-instance-access-form object slot)))
    (cond ((eq value '#.+unbound-slot-value+)
           '("#<unbound>"))
          ((computed-state-p value)
           `(,(if (computed-state-valid-p value) "Valid: " "Invalid: ")
             (:value ,(cs-value value))
             ,(concatenate 'string ", pulse: " (integer-to-string (cs-computed-at-pulse value)) "/" (integer-to-string (cs-validated-at-pulse value)))
             ", " (:value ,value ,(cu-name (cs-universe value)))
             " "
             ,(if (computed-state-valid-p value)
                  `(:action "[invalidate]" ,(lambda () (invalidate-computed-state value)))
                  `(:action "[compute]" ,(lambda () (ensure-computed-state-is-valid value))))))
          (t (call-next-method)))))
