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

#.(file-header)

;;;;;;;;;;;;;;;;;;;;
;;; Public interface

(defmacro defcclass (name superclasses slots &rest options)
  `(defclass ,name ,superclasses , slots
    ,@(append (unless (find :metaclass options :key 'first)
                '((:metaclass computed-class)))
              options)))

(defmacro define-computed-universe (compute-as-macro-name &key (name (let ((*package* (find-package "KEYWORD")))
                                                                       (format nil "~S" compute-as-macro-name)))
                                                          (default-recomputation-mode :on-demand)
                                                          (self-variable-name '-self-)
                                                          (current-value-variable-name '-current-value-))
  "Use define-computed-universe to define a universe glueing together computed slots. It will define a macro with the given name that can be used to initialize computed slots with a computation."
  ;; mark on the symbol that this is a compute-as macro
  (declare (type symbol compute-as-macro-name))
  (let ((primitive-compute-as-macro-name (concatenate-symbol compute-as-macro-name "*"))
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


(defgeneric computed-value-equal-p (old-value new-value)
  (:documentation "When a new value is set in a computed slot, then this method is used to decide whether dependent slots should be recalculated or not.")
  (:method (old-value new-value)
           #f))

;; TODO these should probably be simple defun's. who would ever want to override them and how? also they are suboptimal.
(defgeneric invalidate-computed-slot (object slot)
  (:documentation "Forces the recalculation of a slot on the next slot-value or accessor call.")
  (:method ((object computed-object) (slot-name symbol))
           (invalidate-computed-slot object (find-slot (class-of object) slot-name)))
  (:method ((object computed-object) (slot computed-effective-slot-definition))
           (let ((computed-state (computed-state-or-nil object slot)))
             (if computed-state
                 (invalidate-computed-state computed-state)
                 (when (slot-boundp-using-class (class-of object) object slot)
                   (error "The slot ~A of ~A is not computed while invalidate-computed-slot was called on it" slot object))))))

(defgeneric make-slot-uncomputed (object slot)
  (:documentation "Makes the slot a constant slot with respect to other computed slots. The current value will not be racalculated even if it's invalid.")
  (:method ((object computed-object) (slot-name symbol))
           (make-slot-uncomputed object (find-slot (class-of object) slot-name)))
  (:method ((object computed-object) (slot computed-effective-slot-definition))
           (let ((computed-state (computed-state-or-nil object slot)))
             (when computed-state
               (setf-standard-instance-access-form (cs-value computed-state) object slot)))))

(defgeneric recompute-slot (object slot)
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
