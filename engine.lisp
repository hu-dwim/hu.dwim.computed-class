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

(def constant +invalid-pulse+ -1
  "The invalid pulse will be set in the computed-state whenever it has to be recomputed on the next read operation.")

;;;;;;;;;;;;;;;;;;;
;;; Computed states

(defstruct (computed-universe (:conc-name cu-))
  "This counter will be incremented each time a computed slot is set either by calling slot-value or by the accessor. On the other hand when a computed slot is recomputed due to changes in the computed slots used when the original slot was last computed then this counter will not change. The first valid pulse value is 0."
  (pulse 0 :type integer)
  (name nil :type (or null string)))

(defparameter *default-universe* (make-computed-universe :name "Default computed universe"))

(defun incf-pulse (computed-state)
  (declare (type computed-state computed-state)
           #+sbcl(sb-ext:muffle-conditions sb-ext:compiler-note))
  ;; TODO think about fixnum overflow...
  (incf (cu-pulse (cs-universe computed-state))))

(defun current-pulse (computed-state)
  (declare (type computed-state computed-state)
           #+sbcl(sb-ext:muffle-conditions sb-ext:compiler-note))
  (cu-pulse (cs-universe computed-state)))

(defstruct (computed-state (:conc-name cs-) (:print-object print-computed-state))
  "Describes the different kind of computed states. The value present in the slot of an object or the value present in a variable."
  (universe
   nil
   :type computed-universe)
  (computed-at-pulse
   +invalid-pulse+
   :type integer)
  (validated-at-pulse
   +invalid-pulse+
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
   :type (or null symbol function)) ; direct values have nil compute-as
  #+debug(form
          nil
          :type (or atom list))
  (kind
   'standalone
   :type (member standalone object-slot variable))
  (recomputation-mode
   :on-demand
   ;; TODO: add :keep-up-to-date
   :type (member :always :on-demand))
  ;; contains the name of the variable
  (variable
   nil
   :type symbol)
  ;; these two are used for object slots
  (object
   nil
   :type (or null computed-object))
  (slot
   nil
   :type (or null computed-effective-slot-definition))
  (value
   nil
   :type t))

(defun copy-place-independent-slots-of-computed-state (from into)
  "Copy the slots of FROM into INTO that are not dependent on the place this computed slot has been assigned to."
  (declare (type computed-state from into))
  (macrolet ((x (&rest names)
               `(progn
                 ,@(loop for name :in names
                         for accessor = (concatenate-symbol (find-package :computed-class) ; *package* is not ok, because due to inlining it may get expanded in a different package
                                                            "cs-" name)
                         collect `(setf (,accessor into) (,accessor from))))))
    (x universe
       computed-at-pulse
       validated-at-pulse
       depends-on
       #+debug depending-on-me
       compute-as
       #+debug form
       value)
    (values)))

(define-dynamic-context recompute-state-contex
  ((computed-state nil
    :type computed-state) ; "The computed-state being recomputed."
   (used-computed-states nil
    :type list)) ; "The computed-states used while recomputing COMPUTED-STATE."
  :chain-parents #t
  :create-struct #t
  :struct-options ((:conc-name rsc-)))


(defun computed-state-value (computed-state)
  "Read the value, recalculate when needed."
  (declare (type computed-state computed-state)
           #.(optimize-declaration))
  (%computed-state-value computed-state))

(defun (setf computed-state-value) (new-value computed-state)
  "Set the value, invalidate and recalculate as needed."
  (declare (type computed-state computed-state)
           #.(optimize-declaration))
  (unless (eq (cs-kind computed-state) 'standalone)
    (error "You may only call (setf computed-state-value) on standalone computed-state's and ~A is not one" computed-state))
  (setf (%computed-state-value computed-state) new-value)
  (setf (cs-compute-as computed-state) nil))

(defun %computed-state-value (computed-state)
  (declare (type computed-state computed-state)
           #.(optimize-declaration))
  (when (has-recompute-state-contex)
    (in-recompute-state-contex context
      (let ((computed-state-being-recomputed (rsc-computed-state context)))
        (when (and (not (eq :always (cs-recomputation-mode computed-state-being-recomputed)))
                   (eq (cs-universe computed-state-being-recomputed)
                       (cs-universe computed-state)))
          (push computed-state (rsc-used-computed-states context))))))
  (ensure-computed-state-is-valid computed-state)
  (cs-value computed-state))

(defun (setf %computed-state-value) (new-value computed-state)
  (declare (type computed-state computed-state)
           #.(optimize-declaration))
  (let ((current-pulse (incf-pulse computed-state)))
    ;; setting cs-depends-on to nil effectively makes it uncomputed, but other computed-state's can still depend on it
    ;; and get invalidated when the value is updated.
    (setf (cs-depends-on computed-state) nil)
    (setf (cs-computed-at-pulse computed-state) current-pulse)
    (setf (cs-validated-at-pulse computed-state) current-pulse)
    (setf (cs-value computed-state) new-value)))

(defun computation-of-computed-state (computed-state)
  (declare (type computed-state computed-state)
           #.(optimize-declaration))
  (cs-compute-as computed-state))

(defun (setf computation-of-computed-state) (new-value computed-state)
  (declare (type computed-state computed-state)
           (type function new-value)
           #.(optimize-declaration))
  (setf (cs-compute-as computed-state) new-value)
  (invalidate-computed-state computed-state #t))

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
      (let* ((old-value (cs-value computed-state))
             (new-value (aif (cs-compute-as computed-state)
                             (funcall it (cs-object computed-state) old-value)
                             (error "Now, this is bad: we encoutered an invalid computed-state ~A which is holding a constant and therefore has no compute-as lambda"
                                    computed-state)))
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

(locally (declare #+sbcl(sb-ext:muffle-conditions sb-ext:compiler-note))
;; TODO: the muffle is only needed because pulse is not fixnum for now
(defun computed-state-valid-p (computed-state)
  (declare (type computed-state computed-state)
           #.(optimize-declaration))
  (if (eq :always (cs-recomputation-mode computed-state))
      (values #f computed-state)
      (let ((computed-at-pulse (cs-computed-at-pulse computed-state))
            (validated-at-pulse (cs-validated-at-pulse computed-state)))
        (log.debug "Validating ~A" computed-state)
        (multiple-value-bind (valid-p newer-computed-state)
            (block valid-check
              (when (= (current-pulse computed-state) validated-at-pulse)
                (return-from valid-check (values #t nil)))
              (when (= computed-at-pulse #.+invalid-pulse+)
                (return-from valid-check (values #f computed-state)))
              (loop for depends-on-computed-state :in (cs-depends-on computed-state) do
                    (log.debug "Comparing ~A to ~A" computed-state depends-on-computed-state)
                    (if (>= computed-at-pulse (cs-computed-at-pulse depends-on-computed-state))
                        (multiple-value-bind (valid-p newer-computed-state)
                            (computed-state-valid-p depends-on-computed-state)
                          (unless valid-p
                            (return-from valid-check (values #f newer-computed-state))))
                        (return-from valid-check (values #f depends-on-computed-state))))
              (values #t nil))
          (declare (type boolean valid-p)
                   (type (or null computed-state) newer-computed-state))
          (if valid-p
              (setf (cs-validated-at-pulse computed-state) (current-pulse computed-state))
              (progn
                (log.debug "Value turned out to be invalid for ~A" computed-state)
                (invalidate-computed-state computed-state #t)))
          (values valid-p newer-computed-state))))))

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
                          (append (list computed-state)
                                  (loop for parent-context = context :then (rsc-parent-context parent-context)
                                        while parent-context
                                        collect (rsc-computed-state parent-context))))))))))

(defun invalidate-computed-state (computed-state &optional locally)
  "Invalidate the given COMPUTED-STATE. When LOCALLY is #t then this invalidation has only local effects on this computed-state and the dependent computed-states are not invalidated."
  (declare (type computed-state computed-state))
  (unless locally
    (incf-pulse computed-state))
  (setf (cs-computed-at-pulse computed-state) #.+invalid-pulse+)
  (setf (cs-validated-at-pulse computed-state) #.+invalid-pulse+))

(defun print-computed-state (computed-state stream)
  (declare (type computed-state computed-state))
  (let* ((name (if (eq (cs-kind computed-state) 'object-slot)
                   (awhen (cs-slot computed-state)
                     (slot-definition-name it))
                   (cs-variable computed-state))))
    (when (eq (cs-kind computed-state) 'object-slot)
      (format stream "~A / " (cs-object computed-state)))
    (format stream "<#~A :pulse ~A :value ~A :kind ~A>"
            name (cs-computed-at-pulse computed-state) (cs-value computed-state) (cs-kind computed-state))))

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
  (let ((slot (find slot-name (the list (class-slots class)) :key #'slot-definition-name :test #'eq)))
    (assert slot () "Slot ~S not found in class ~S" slot-name class)
    slot))

(defun computed-state-or-nil (object slot)
  (declare (type computed-object object)
           (type computed-effective-slot-definition slot)
           #.(optimize-declaration))
  (the (or null computed-state)
    (let ((result (standard-instance-access-form object slot)))
      (when (and (not (eq result '#.+unbound-slot-value+))
                 (computed-state-p result))
        result))))

(defun compute-as-form-p (form)
  "To identify forms that create a computed state, IOW all kind of (compute-as ...) forms."
  (and (consp form)
       (symbolp (first form))
       (get (first form) 'computed-as-macro-p)))

(defun primitive-compute-as-form-p (form)
  "To identify (compute-as* ...) forms, that are the primitive computed state factories of a computed universe."
  (and (compute-as-form-p form)
       (eq (first form) (get (first form) 'primitive-compute-as-macro))))

(defun primitive-compute-as-form-of (input-form &optional env)
  (if (primitive-compute-as-form-p input-form)
      input-form
      (loop with form = input-form
            with expanded-p = #t
            while (progn
                    (multiple-value-setq (form expanded-p) (macroexpand-1 form env))
                    expanded-p)
            do (when (primitive-compute-as-form-p form)
                 (return form))
            finally (error "Form ~S can not be macroexpanded into a primitive-compute-as-form-p. This should not happen." input-form))))

(defun ensure-arguments-for-primitive-compute-as-form (form &rest args)
  (let ((form-args (copy-list (second form))))
    (loop for (indicator value) :on args :by #'cddr do
          (setf (getf form-args indicator) value))
    `(,(first form) ,form-args
      ,@(cddr form))))

