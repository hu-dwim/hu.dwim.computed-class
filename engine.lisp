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

(eval-always
  (defconstant +invalid-pulse+ -1
    "The invalid pulse will be set in the computed-state whenever it has to be recomputed on the next read operation."))

;;;;;;;;;;;;;;;;;;;
;;; Computed states

(defstruct (computed-universe (:conc-name cu-))
  "This counter will be incremented each time a computed slot is set either by calling slot-value or by the accessor. On the other hand when a computed slot is recomputed due to changes in the computed slots used when the original slot was last computed then this counter will not change. The first valid pulse value is 0."
  (pulse 0 :type integer)
  (name nil :type (or null string)))

(defparameter *default-universe* (make-computed-universe :name "Default computed universe"))

(defun incf-pulse (computed-state)
  (declare (type computed-state computed-state))
  (incf (cu-pulse (cs-universe computed-state))))

(defun current-pulse (computed-state)
  (declare (type computed-state computed-state))
  (cu-pulse (cs-universe computed-state)))

(defstruct (computed-state (:conc-name cs-) (:print-object print-computed-state))
  "Describes the different kind of computed states. The value present in the slot of an object or the value present in a variable."
  (universe nil :type computed-universe)
  (computed-at-pulse
   #.+invalid-pulse+
   :type integer)
  (validated-at-pulse
   #.+invalid-pulse+
   :type integer)
  (depends-on
   nil
   :type list) ; of computed-state's
  ;;(depending-on-me but only those that need to be notified) TODO: not yet implemented
  #+debug(depending-on-me
          ;; TODO keep a full list for debug purposes
          nil
          :type list) ; of computed-state's
  (compute-as
   nil
   :type function)
  ;; these two are used for slots
  (object
   'not-an-object-slot-state
   :type (or symbol computed-object))
  (slot
   nil
   :type (or symbol computed-effective-slot-definition))
  (value
   nil
   :type t)
  #+debug(form
          nil
          :type list)
  #+debug(attached-to-object-p
          #f
          :type boolean))

(defun computed-state-object-slot-p (computed-state)
  "Is this computed-state used as a slot of an object?"
  (declare (type computed-state computed-state)
           #.(optimize-declaration))
  (not (eq (cs-object computed-state) 'not-an-object-slot-state)))

(define-dynamic-context recompute-state-contex
  ((computed-state nil
    :type computed-state)
   (used-computed-states nil
    :type list))
  :chain-parents #t
  :create-struct #t
  :struct-options ((:conc-name rsc-)))


(defun computed-state-value (computed-state)
  "Read the value, recalculate when needed."
  (declare (type computed-state computed-state)
           #.(optimize-declaration))
  (ensure-computed-state-is-valid computed-state)
  (cs-value computed-state))

(defun (setf computed-state-value) (new-value computed-state)
  "Set the value, invalidate and recalculate as needed."
  (declare (type computed-state computed-state)
           #.(optimize-declaration))
  (let ((current-pulse (incf-pulse computed-state))) 
    (setf (cs-depends-on computed-state) nil)
    (setf (cs-computed-at-pulse computed-state) current-pulse)
    (setf (cs-validated-at-pulse computed-state) current-pulse)
    (setf (cs-value computed-state) new-value)))


(defun ensure-computed-state-is-valid (computed-state)
  (declare (type computed-state computed-state)
           #.(optimize-declaration))
  (multiple-value-bind (valid-p newer-computed-state)
      (computed-state-valid-p computed-state)
    (declare (ignorable newer-computed-state))
    (unless valid-p
      (log.debug "About to recompute ~A because ~A is newer" computed-state newer-computed-state)
      (check-circularity computed-state)
      (recompute-computed-state computed-state)))
  (values))

(defun recompute-computed-state (computed-state)
  (declare (type computed-state computed-state)
           #.(optimize-declaration))
  (with-new-recompute-state-contex (:computed-state computed-state)
    (in-recompute-state-contex context
      (log.debug "Recomputing slot ~A" computed-state)
      (let* ((object (cs-object computed-state))
             (old-value (cs-value computed-state))
             (new-value (funcall (cs-compute-as computed-state) object old-value))
             (store-new-value-p (if (and (primitive-p old-value)
                                         (primitive-p new-value))
                                    (not (equal old-value new-value))
                                    (not (computed-value-equal-p old-value new-value)))))
        (setf (cs-depends-on computed-state) (rsc-used-computed-states context))
        (when (or store-new-value-p
                  (= (cs-computed-at-pulse computed-state) #.+invalid-pulse+))
          (setf (cs-computed-at-pulse computed-state) (current-pulse computed-state))
          (setf (cs-validated-at-pulse computed-state) (current-pulse computed-state)))
        (if store-new-value-p
            (setf (cs-value computed-state) new-value)
            (log.debug "Not storing fresh recomputed value for ~A because it was equal to the cached value." computed-state))
        new-value))))

(defun computed-state-valid-p (computed-state)
  (declare (type computed-state computed-state)
           #.(optimize-declaration))
  (let ((computed-at-pulse (cs-computed-at-pulse computed-state))
        (validated-at-pulse (cs-validated-at-pulse computed-state)))
    (log.debug "Validating ~A" computed-state)
    (multiple-value-bind (valid-p newer-computed-state)
        (block valid-check
          (when (= (current-pulse computed-state) validated-at-pulse)
            (return-from valid-check (values #t nil)))
          (when (= computed-at-pulse #.+invalid-pulse+)
            (return-from valid-check (values #f computed-state)))
          (loop for depends-on-computed-state :in (cs-depends-on computed-state)
                do (let* ((used-object (cs-object depends-on-computed-state))
                          (used-slot (cs-slot depends-on-computed-state))
                          (current-depends-on-computed-state (if (computed-state-object-slot-p depends-on-computed-state)
                                                                 (computed-state-or-nil used-object used-slot)
                                                                 depends-on-computed-state))
                          (current-used-computed-at-pulse
                           (when current-depends-on-computed-state
                             (cs-computed-at-pulse current-depends-on-computed-state))))
                     (log.debug "Comparing ~A to ~A" computed-state current-depends-on-computed-state)
                     (when current-used-computed-at-pulse
                       (if (>= computed-at-pulse current-used-computed-at-pulse)
                           (multiple-value-bind (valid-p newer-computed-state)
                               (computed-state-valid-p current-depends-on-computed-state)
                             (unless valid-p
                               (return-from valid-check (values #f newer-computed-state))))
                           (return-from valid-check (values #f current-depends-on-computed-state))))))
          (values #t nil))
      (declare (type boolean valid-p)
               (type (or null computed-state) newer-computed-state))
      (if valid-p
          (setf (cs-validated-at-pulse computed-state) (current-pulse computed-state))
          (progn
            (log.debug "Value turned out to be invalid for ~A" computed-state)
            (invalidate-computed-state computed-state)))
      (values valid-p newer-computed-state))))

(defun check-circularity (computed-state)
  (declare (type computed-state computed-state)
           #.(optimize-declaration))
  (when (has-recompute-state-contex)
    (in-recompute-state-contex context
      (loop for parent-context = context :then (rsc-parent-context parent-context)
            while parent-context
            do (let ((parent-computed-state (rsc-computed-state parent-context)))
                 (when (eq computed-state parent-computed-state)
                   (error "Circularity detected among the computed slots/variables ~A"
                          (loop for parent-context = context :then (rsc-parent-context parent-context)
                                while parent-context
                                collect (cs-slot (rsc-computed-state parent-context))))))))))

(defun invalidate-computed-state (computed-state)
  (declare (type computed-state computed-state))
  (setf (cs-computed-at-pulse computed-state) #.+invalid-pulse+)
  (setf (cs-validated-at-pulse computed-state) #.+invalid-pulse+))

(defun print-computed-state (computed-state stream)
  (declare (type computed-state computed-state))
  (let* ((object (cs-object computed-state))
         (slot (cs-slot computed-state))
         (slot-name (if (and slot
                             (typep slot 'slot-definition))
                        (slot-definition-name slot)
                        slot))
         (attached-p #+debug(if (cs-attached-to-object-p computed-state)
                                "#t"
                                "#f")
                     #-debug "n/a"))
    (if (eq object 'not-an-object-slot-state)
        (format stream "~A : <#~A :pulse ~A>"
                (cs-value computed-state) slot-name (cs-computed-at-pulse computed-state))
        (format stream "~A : ~A / <#~A :pulse ~A :attached ~A>"
                (cs-value computed-state) object slot-name (cs-computed-at-pulse computed-state) attached-p))))





;;;;;;;;;;;;;;;;;;
;;; Helper methods

(defun primitive-p (object)
  (or (numberp object)
      (stringp object)
      (symbolp object)
      (characterp object)))

(defun find-slot (class slot-name)
  (declare (type standard-class class)
           (type symbol slot-name)
           #.(optimize-declaration))
  (find slot-name (the list (class-slots class)) :key #'slot-definition-name :test #'eq))

(defun computed-state-or-nil (object slot)
  (declare (type (or symbol computed-object) object)
           (type (or null computed-effective-slot-definition) slot)
           #.(optimize-declaration))
  (the (or null computed-state)
    (let ((result #.(standard-instance-access-form)))
      (when (and (not (eq result '#.+unbound-slot-value+))
                 (computed-state-p result))
        result))))


