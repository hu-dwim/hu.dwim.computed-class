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
  (bind ((slot-value (standard-instance-access-form object slot)))
    (cond ((eq slot-value '#.+unbound-slot-value+)
           '("#<unbound>"))
          ((computed-state-p slot-value)
           `(,(if (computed-state-valid-p slot-value) "Valid: " "Invalid: ")
             (:value ,(cs-value slot-value))
             ,(concatenate 'string ", pulse: " (integer-to-string (cs-computed-at-pulse slot-value)) "/" (integer-to-string (cs-validated-at-pulse slot-value)))
             ", " (:value ,slot-value)
             " "
             ,(if (computed-state-valid-p slot-value)
                  `(:action "[invalidate]" ,(lambda () (invalidate-computed-state slot-value)))
                  `(:action "[compute]" ,(lambda () (ensure-computed-state-is-valid slot-value))))))
          (t (call-next-method)))))
