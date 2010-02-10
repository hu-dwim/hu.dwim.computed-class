;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.computed-class)

(def (macro e) defcclass (name superclasses slots &rest options)
  `(defclass ,name ,superclasses , slots
    ,@(append (unless (find :metaclass options :key 'first)
                '((:metaclass computed-class)))
              options)))

(def (definer e :available-flags "e") computed-class (name superclasses slots &rest options)
  `(progn
     (defcclass ,name ,superclasses ,slots ,@options)
     ,@(when (getf -options- :export)
         `((export ',name)))))

(def method invalidate-computed-slot ((object computed-object) (slot computed-effective-slot-definition))
  (let ((computed-state (computed-state-or-nil object slot)))
    (if computed-state
        (invalidate-computed-state computed-state)
        (when (slot-boundp-using-class (class-of object) object slot)
          (error "The slot ~A of ~A is not computed while invalidate-computed-slot was called on it" slot object)))))

(def method computed-slot-valid-p ((object computed-object) (slot computed-effective-slot-definition))
  (let ((computed-state (computed-state-or-nil object slot)))
    (if computed-state
        (computed-state-valid-p computed-state)
        #t)))

(def method make-slot-uncomputed ((object computed-object) (slot computed-effective-slot-definition))
  (let ((computed-state (computed-state-or-nil object slot)))
    (when computed-state
      (setf-standard-instance-access-form (cs-value computed-state) object slot))))

(def method recompute-slot ((object computed-object) (slot computed-effective-slot-definition))
  (let ((computed-state (computed-state-or-nil object slot)))
    (if computed-state
        (recompute-computed-state computed-state)
        (if (slot-boundp-using-class (class-of object) object slot)
            (error "The slot ~A of ~A is not computed while recompute-slot was called on it" slot object)
            (slot-unbound (class-of object) object (slot-definition-name slot))))))
