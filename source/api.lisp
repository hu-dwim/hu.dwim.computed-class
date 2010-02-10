;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.computed-class)

(def (class e) computed-class (standard-class)
  ()
  (:documentation "A computed class might have slots which are computed based on other computed slots in other computed class instances. A slot of a computed class is either a standard slot or a computed slot and only class redefinition may change this. Slots which are computed will be tracked, invalidated and/or recomputed whenever a computed slot value changes which were used last time when the slot was computed. The used computed slots are collected runtime and per instance. Moreover different instances might compute the same slots in different ways."))

(def (class e) computed-class* (computed-class)
  ()
  (:documentation "Just like computed-class but the classes having this metaclass will have custom accessors. This slows down loading but speeds up the accessors quite a bit."))

(def (class e) computed-object ()
  ()
  (:documentation "This is the base class for all computed classes. The class need not be listed in the direct supers when defining a computed class because the metaclass makes sure it's among them."))

(def (generic e) computed-value-equal? (old-value new-value)
  (:documentation "When a new value is set in a computed slot, then this method is used to decide whether dependent slots should be recalculated or not.")
  (:method (old-value new-value)
    #f))

;; maybe these should be simple defun's? who would ever want to override them and how?
(def (generic e) invalidate-computed-slot (object slot)
  (:documentation "Forces the recalculation of a slot on the next slot-value or accessor call.")
  (:method ((object computed-object) (slot-name symbol))
    (invalidate-computed-slot object (find-slot (class-of object) slot-name))))

(def (generic e) computed-slot-valid-p (object slot)
  (:documentation "Checks if the given slot value is invalid or not.")
  (:method ((object computed-object) (slot-name symbol))
    (computed-slot-valid-p object (find-slot (class-of object) slot-name))))

(def (generic e) make-slot-uncomputed (object slot)
  (:documentation "Makes the slot a constant slot with respect to other computed slots. The current value will not be racalculated even if it's invalid.")
  (:method ((object computed-object) (slot-name symbol))
    (make-slot-uncomputed object (find-slot (class-of object) slot-name))))

(def (generic e) recompute-slot (object slot)
  (:documentation "Enforces the recomputation of the given slot.")
  (:method ((object computed-object) (slot-name symbol))
    (recompute-slot object (find-slot (class-of object) slot-name))))
