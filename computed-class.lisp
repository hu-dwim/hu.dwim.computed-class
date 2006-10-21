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

(declaim (optimize (speed 3) (debug 0) (safety 0)))

(enable-sharp-boolean-syntax)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CLOS class and slot meta objects

(defclass computed-class (standard-class)
  ()
  (:documentation "A computed class might have slots which are computed based on other computed slots in other computed class instances. A slot of a computed class is either a standard slot or a computed slot and only class redefinition may change this. Slots which are computed will be tracked, invalidated and/or recomputed whenever a computed slot value changes which were used last time when the slot was computed. The used computed slots are collected runtime and per instance. Moreover different instances might compute the same slots in different ways."))

(defclass computed-slot-definition (standard-slot-definition)
  ((compute-as
    :type computed-state
    :accessor compute-as-of
    :initarg :compute-as)))

(defclass computed-direct-slot-definition (computed-slot-definition standard-direct-slot-definition)
  ())

(defclass computed-effective-slot-definition (computed-slot-definition standard-effective-slot-definition)
  ())

(defclass computed-object ()
  ((slot-computed-state-pairs
    :initform nil
    :type list
    :accessor slot-computed-state-pairs-of
    :initarg :computed-states
    :documentation "An alist of slot meta objects and corresponding computed-state structure objects."))
  (:documentation "This is the base class for all computed classes. The class need not be listed in the direct classes when defining a computed class because the meta class takes care adding it."))

;;;;;;;;;;;;;;;;;;;
;;; Computed states

(defparameter *pulse* 0
  "This counter will be incremented each time a computed slot is set either by calling slot-value or by the accessor. On the other hand when a computed slot is recomputed due to changes in the computed slots used when the original slot was last computed then this counter will not change. The first valid pulse value is 0.")
(declaim (type integer *pulse*))

(defun incf-pulse ()
  (incf *pulse*))

(defparameter *bypass-computed-slot-value-using-class* #f
  "This specifies whether slot-value and friends should bypass the computed slot behaviour and fallback to the standard behavior.")
(declaim (type boolean *bypass-computed-slot-value-using-class*))

(defconstant +invalid-pulse+ -1
  "The invalid pulse will be set in the computed-state whenever it has to be recomputed on the next read operation.")

(defstruct computed-state
  "Describes the different kind of computed states. The value present in the slot of an object or the value present in a variable."
  (pulse
   -1
   :type integer)
  (used-computed-states
   nil
   :type list)
  ;; TODO: not yet implemented
  (user-computed-states
   nil
   :type list)
  (form
   nil
   :type list)
  (compute-as
   nil
   :type function)
  ;; these two are used for slots
  (object
   nil
   :type (or null computed-object))
  (slot
   nil
   :type (or null computed-effective-slot-definition))
  ;; this is used for variables
  ;; TODO: not yet implemented
  (value
   nil
   :type t))

;; TODO: use the bind package and struct here for performance reasons 
(define-dynamic-context recompute-slot-value-contex
  ((used-computed-states
    :initform nil
    :type list
    :accessor used-computed-states-of
    :initarg :used-computed-states)
   (object
    :type computed-object
    :accessor object-of
    :initarg :object)
   (slot
    :type computed-effective-slot-definition
    :accessor slot-of
    :initarg :slot))
  :chain-parents #t)

;;;;;;;;;;;;;;;;;;;;
;;; CLOS MOP related

(defmethod validate-superclass ((class standard-class) (superclass computed-class))
  t)

(defmethod validate-superclass ((class computed-class) (superclass standard-class))
  t)

(defmethod direct-slot-definition-class ((class computed-class) &key name initform (computed #f) &allow-other-keys)
  (if (and (or (eq name 'computed-states)
               (not computed))
           (or (not (consp initform))
               (not (eq (car initform) 'compute-as))))
      (call-next-method)
      (find-class 'computed-direct-slot-definition)))

(defmethod effective-slot-definition-class ((class computed-class) &key &allow-other-keys)
  (declare (special %computed-effective-slot-definition%))
  (if %computed-effective-slot-definition%
      (find-class 'computed-effective-slot-definition)
      (call-next-method)))

(defmethod compute-effective-slot-definition :around ((class computed-class) name direct-slot-definitions)
  (declare (type list direct-slot-definitions))
  (let ((%computed-effective-slot-definition% (find-if (lambda (direct-slot-definition)
                                                         (typep direct-slot-definition 'computed-direct-slot-definition))
                                                       direct-slot-definitions)))
    (declare (special %computed-effective-slot-definition%))
    (call-next-method)))

(defgeneric cached-slot-value (object slot-name)
  (:method ((object computed-object) (slot-name symbol))
           (let ((*bypass-computed-slot-value-using-class* #t))
             (slot-value object slot-name))))

(defgeneric (setf cached-slot-value) (new-value object slot-name)
  (:method (new-value (object computed-object) (slot-name symbol))
           (let ((*bypass-computed-slot-value-using-class* #t))
             (setf (slot-value object slot-name) new-value))))

(defgeneric cached-slot-value-using-class (class object slot)
  (:method ((class computed-class) (object computed-object) (slot computed-effective-slot-definition))
           (let ((*bypass-computed-slot-value-using-class* #t))
             (slot-value-using-class class object slot))))

(defgeneric (setf cached-slot-value-using-class) (new-value class object slot)
  (:method (new-value (class computed-class) (object computed-object) (slot computed-effective-slot-definition))
           (let ((*bypass-computed-slot-value-using-class* #t))
             (setf (slot-value-using-class class object slot) new-value))))

(defgeneric cached-slot-boundp-using-class (class object slot)
  (:method ((class computed-class) (object computed-object) (slot computed-effective-slot-definition))
           (let ((*bypass-computed-slot-value-using-class* #t))
             (slot-boundp-using-class class object slot))))

(defmethod slot-value-using-class :before ((class computed-class)
                                           (object computed-object)
                                           (slot computed-effective-slot-definition))
  (let ((computed-state (computed-state-for object slot)))
    (when (and computed-state
               (has-recompute-slot-value-contex))
      (in-recompute-slot-value-contex context
        (push computed-state (used-computed-states-of context))))
    (unless (or *bypass-computed-slot-value-using-class*
                (not computed-state))
      (ensure-valid-slot-value class object slot computed-state))))

(defmethod (setf slot-value-using-class) (new-value
                                          (class computed-class)
                                          (object computed-object)
                                          (slot computed-effective-slot-definition))
  (let ((computed-state (computed-state-for object slot)))
    (cond (*bypass-computed-slot-value-using-class*
           (call-next-method))
          ((not (typep new-value 'computed-state))
           (when computed-state
             (setf (computed-state-used-computed-states computed-state) nil)
             (setf (computed-state-pulse computed-state) (incf-pulse)))
           (call-next-method))
          (t
           (setf (computed-state-object new-value) object)
           (setf (computed-state-slot new-value) slot)
           (setf (computed-state-pulse new-value) +invalid-pulse+)
           (setf (computed-state-for object slot) new-value)
           (incf-pulse)))))

(defmethod slot-boundp-using-class ((class computed-class)
                                    (object computed-object)
                                    (slot computed-effective-slot-definition))
  (if *bypass-computed-slot-value-using-class*
      (call-next-method)
      (or (computed-state-for object slot)
          (call-next-method))))

(defmethod shared-initialize :around ((object computed-class) slot-names &rest args
                                      &key direct-superclasses &allow-other-keys)
  (remf-keywords args :direct-superclasses)
  (let* ((computed-object (find-class 'computed-object))
         (direct-superclasses (if (member computed-object direct-superclasses :test 'eq)
                                  direct-superclasses
                                  (append direct-superclasses (list computed-object)))))
    (apply #'call-next-method object slot-names :direct-superclasses direct-superclasses args)))

(defmethod shared-initialize :before ((object computed-slot-definition) slot-names &key computed &allow-other-keys)
  (declare (ignore object slot-names computed)))

(defmethod initialize-instance :before ((object computed-object) &key &allow-other-keys)
  (setf (slot-computed-state-pairs-of object) nil))

;;;;;;;;;;;;;;;;;;
;;; Helper methods

(defun primitive-p (object)
  (declare (inline))
  (or (numberp object)
      (stringp object)
      (symbolp object)
      (characterp object)))

(defun find-slot (class slot-name)
  (declare (type standard-class class)
           (type symbol slot-name))
  (find slot-name (the list (class-slots class)) :key 'slot-definition-name))

(defun ensure-valid-slot-value (class object slot computed-state)
  (declare (type computed-class class)
           (type computed-object object)
           (type computed-effective-slot-definition slot)
           (type computed-state computed-state)
           (optimize (speed 3) (debug 0) (safety 0)))
  (unless (slot-value-valid-p object slot)
    (check-circularity object slot)
    (recompute-slot-value class object slot computed-state))
  (values))

(defun recompute-slot-value (class object slot computed-state)
  (declare (type computed-class class)
           (type computed-object object)
           (type computed-effective-slot-definition slot)
           (type computed-state computed-state))
  (let ((context (make-instance 'recompute-slot-value-contex :object object :slot slot)))
    (with-recompute-slot-value-contex context
      (log.debug "Recomputing object ~A for slot ~A with pulse ~A" object slot (computed-state-pulse computed-state))
      (let ((new-value (funcall (computed-state-compute-as computed-state) object))
            (store-new-value-p #t))
        (setf (computed-state-used-computed-states computed-state)
              (used-computed-states-of context))
        (when (cached-slot-boundp-using-class class object slot)
          (let ((old-value (cached-slot-value-using-class class object slot)))
            (setf store-new-value-p
                  (if (and (primitive-p old-value)
                           (primitive-p new-value))
                      (not (equal old-value new-value))
                      (not (computed-value-equal-p old-value new-value))))))
        (when store-new-value-p
          (setf (cached-slot-value-using-class class object slot) new-value)
          (setf (computed-state-pulse computed-state) *pulse*))
        new-value))))

(defun slot-value-valid-p (object slot)
  (declare (type computed-object object)
           (type computed-effective-slot-definition slot))
  (let* ((computed-state (computed-state-for object slot))
         (pulse (computed-state-pulse computed-state)))
    (log.debug "Validating object ~A for slot ~A with pulse: ~A" object slot pulse)
    (let ((result
           (or (= *pulse* pulse)
               (and (not (= pulse +invalid-pulse+))
                    (or (not computed-state)
                        (every (lambda (computed-state)
                                 (let* ((used-object (computed-state-object computed-state))
                                        (used-slot (computed-state-slot computed-state))
                                        (used-computed-state (computed-state-for used-object used-slot))
                                        (used-pulse (when used-computed-state
                                                      (computed-state-pulse used-computed-state))))
                                   (log.debug "Comparing object ~A for slot ~A with pulse ~A to object ~A for slot ~A with pulse ~A"
                                              object slot pulse used-object used-slot used-pulse)
                                   (or (not used-computed-state)
                                       (and (> pulse used-pulse)
                                            (slot-value-valid-p used-object used-slot)))))
                               (computed-state-used-computed-states computed-state)))))))
      (declare (type boolean result))
      (unless result
        (log.debug "Slot value invalid in object ~A for slot ~A" object slot)
        (invalidate-computed-slot object slot))
      result)))

(defun check-circularity (object slot)
  (declare (type computed-object object)
           (type computed-effective-slot-definition slot))
  (when (has-recompute-slot-value-contex)
    (in-recompute-slot-value-contex parent-context
      (loop for ancestor-context = parent-context :then (parent-context-of ancestor-context)
            while ancestor-context
            do
            (unless (or (not (eq object (object-of ancestor-context)))
                        (not (eq slot (slot-of ancestor-context))))
              (error "Circularity detected among computed slots"))))))

;;;;;;;;;;;;;;;;;;;;
;;; Public interface

(defmacro compute-as (&body form)
  "Use this macro when setting the value of a computed slot to a computation."
  `(make-computed-state :form ',form :compute-as (lambda (self) (declare (ignorable self)) ,@form)))

(defgeneric computed-value-equal-p (old-value new-value)
  (:documentation "When a new value is set in a computed slot, then this method is used to decide whether dependent slots should be recalculated or not.")
  (:method (old-value new-value)
           #f))

(defgeneric invalidate-computed-slot (object slot)
  (:documentation "Forces the recalculation of a slot on the next slot-value or accessor call.")
  (:method ((object computed-object) (slot-name symbol))
           (invalidate-computed-slot object (find-slot (class-of object) slot-name)))
  (:method ((object computed-object) (slot computed-effective-slot-definition))
           (let ((computed-state (computed-state-for object slot)))
             (setf (computed-state-pulse computed-state) +invalid-pulse+))))

(defgeneric make-slot-uncomputed (object slot)
  (:documentation "Makes the slot a constant slot with respect to other computed slots.")
  (:method ((object computed-object) (slot-name symbol))
           (make-slot-uncomputed object (find-slot (class-of object) slot-name)))
  (:method ((object computed-object) (slot computed-effective-slot-definition))
           (remove-computed-state-for object slot)))

(defgeneric recompute-slot (object slot)
  (:documentation "Enforces the recomputation of the given slot")
  (:method ((object computed-object) (slot-name symbol))
           (recompute-slot object (find-slot (class-of object) slot-name)))
  (:method ((object computed-object) (slot computed-effective-slot-definition))
           (recompute-slot-value (class-of object) object slot (computed-state-for object slot))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Managing computed slot state 

(defun computed-state-for (object slot)
  (declare (type computed-object object)
           (type computed-effective-slot-definition slot)
           (optimize (speed 3) (debug 0) (safety 0)))
  ;; the assoc turned out to be 2-3 times slower than a loop and this function is key for performance
  ;; (the (or null computed-state) (cdr (assoc slot (the list (slot-computed-state-pairs-of object)) :test 'eq))))
  (the (or null computed-state)
    (let ((computed-states (slot-computed-state-pairs-of object)))      
      (loop for el of-type cons in computed-states
            when (eq (the computed-effective-slot-definition (car el)) slot)
            do (return (cdr el))))))

(defun (setf computed-state-for) (new-value object slot)
  (declare (type computed-state new-value)
           (type computed-object object)
           (type computed-effective-slot-definition slot))
  (aif (assoc slot (slot-computed-state-pairs-of object) :test 'eq)
       (setf (cdr it) new-value)
       (push (cons slot new-value) (slot-computed-state-pairs-of object)))
  (values))

(defun remove-computed-state-for (object slot)
  (declare (type computed-object object)
           (type computed-effective-slot-definition slot))
  (setf (slot-computed-state-pairs-of object)
        (delete slot (the list (slot-computed-state-pairs-of object)) :key 'car :test 'eq))
  (values))
