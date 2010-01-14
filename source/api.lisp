;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.computed-class)

;;;;;;
;;; Public interface

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

(def (macro e) define-computed-universe (compute-as-macro-name &key (name (let ((*package* (find-package "KEYWORD")))
                                                                            (format nil "~S" compute-as-macro-name)))
                                                               (default-recomputation-mode :on-demand)
                                                               (self-variable-name '-self-)
                                                               (current-value-variable-name '-current-value-))
  "Use define-computed-universe to define a universe glueing together computed slots. It will define a macro with the given name that can be used to initialize computed slots with a computation."
  ;; mark on the symbol that this is a compute-as macro
  (declare (type symbol compute-as-macro-name))
  (let ((primitive-compute-as-macro-name (symbolicate compute-as-macro-name "*"))
        (docstring (concatenate 'string "Use this macro to set the value of a computed slot to a computation in the universe '" (string name) "'.")))
    `(eval-always
      (setf (get ',compute-as-macro-name 'computed-as-macro-p) t)
      (setf (get ',compute-as-macro-name 'primitive-compute-as-macro) ',primitive-compute-as-macro-name)

      (setf (get ',primitive-compute-as-macro-name 'computed-as-macro-p) t)
      (setf (get ',primitive-compute-as-macro-name 'primitive-compute-as-macro) ',primitive-compute-as-macro-name)

      (unless (get ',primitive-compute-as-macro-name 'computed-universe)
        (setf (get ',primitive-compute-as-macro-name 'computed-universe) (make-computed-universe :name ,name)))
      (defmacro ,primitive-compute-as-macro-name ((&key (kind 'object-slot) (recomputation-mode ',default-recomputation-mode) (universe)) &body form)
        ,docstring
        (let ((self-variable-name ',self-variable-name))
          (unless (eq kind 'object-slot)
            (setf self-variable-name (gensym)))
          `(make-computed-state :universe ,(or universe
                                               `(load-time-value
                                                (get ',',primitive-compute-as-macro-name 'computed-universe)))
            :recomputation-mode ',recomputation-mode
            #+debug :form #+debug ',form
            :compute-as (lambda (,self-variable-name
                                 ,',current-value-variable-name)
                          (declare (ignorable ,self-variable-name
                                              ,',current-value-variable-name))
                          ,@form)
            :kind ',kind)))
      (defmacro ,compute-as-macro-name (&body body)
        ,docstring
        `(,',primitive-compute-as-macro-name ()
          ,@body)))))

(def (definer e :available-flags "e") computed-universe (compute-as-macro-name &rest args &key name default-recomputation-mode self-variable-name current-value-variable-name)
  (declare (ignore name default-recomputation-mode self-variable-name current-value-variable-name))
  `(define-computed-universe ,compute-as-macro-name ,@args))

(def generic computed-value-equal-p (old-value new-value)
  (:documentation "When a new value is set in a computed slot, then this method is used to decide whether dependent slots should be recalculated or not.")
  (:method (old-value new-value)
    #f))

;; TODO these should probably be simple defun's. who would ever want to override them and how? also they are suboptimal.
(def (generic e) invalidate-computed-slot (object slot)
  (:documentation "Forces the recalculation of a slot on the next slot-value or accessor call.")
  (:method ((object computed-object) (slot-name symbol))
    (invalidate-computed-slot object (find-slot (class-of object) slot-name)))
  (:method ((object computed-object) (slot computed-effective-slot-definition))
    (let ((computed-state (computed-state-or-nil object slot)))
      (if computed-state
          (invalidate-computed-state computed-state)
          (when (slot-boundp-using-class (class-of object) object slot)
            (error "The slot ~A of ~A is not computed while invalidate-computed-slot was called on it" slot object))))))

(def (generic e) computed-slot-valid-p (object slot)
  (:documentation "Checks if the given slot value is invalid or not.")
  (:method ((object computed-object) (slot-name symbol))
    (invalidate-computed-slot object (find-slot (class-of object) slot-name)))
  (:method ((object computed-object) (slot computed-effective-slot-definition))
    (let ((computed-state (computed-state-or-nil object slot)))
      (if computed-state
          (computed-state-valid-p computed-state)
          #t))))

(def (generic e) make-slot-uncomputed (object slot)
  (:documentation "Makes the slot a constant slot with respect to other computed slots. The current value will not be racalculated even if it's invalid.")
  (:method ((object computed-object) (slot-name symbol))
    (make-slot-uncomputed object (find-slot (class-of object) slot-name)))
  (:method ((object computed-object) (slot computed-effective-slot-definition))
    (let ((computed-state (computed-state-or-nil object slot)))
      (when computed-state
        (setf-standard-instance-access-form (cs-value computed-state) object slot)))))

(def (generic e) recompute-slot (object slot)
  (:documentation "Enforces the recomputation of the given slot.")
  (:method ((object computed-object) (slot-name symbol))
    (recompute-slot object (find-slot (class-of object) slot-name)))
  (:method ((object computed-object) (slot computed-effective-slot-definition))
    (let ((computed-state (computed-state-or-nil object slot)))
      (if computed-state
          (recompute-computed-state computed-state)
          (if (slot-boundp-using-class (class-of object) object slot)
              (error "The slot ~A of ~A is not computed while recompute-slot was called on it" slot object)
              (slot-unbound (class-of object) object (slot-definition-name slot)))))))
