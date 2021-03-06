;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.computed-class)

;;;;;;
;;; Constants

(def constant +invalid-pulse+ -1
  "The invalid pulse will be set in the computed-state whenever it has to be recomputed on the next read operation.")

;;;;;;
;;; Computed state

(def type recomputation-mode ()
  ;; TODO: add :keep-up-to-date
  `(member :always :on-demand))

;; TODO keep track of depends-on-me for debugging purposes, and also for forward pushing changes (which is not yet implemented)
(def structure (computed-state (:conc-name cs-) (:print-object print-computed-state))
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
  (compute-as
   nil
   :type (or null symbol function)) ; direct values have nil compute-as
  ;; these two are used for object slots
  (object
   nil
   :type (or null computed-object))
  (value
   nil
   :type t)
  (kind
   'standalone
   :type (member standalone object-slot variable))
  (recomputation-mode
   :on-demand
   :type recomputation-mode)
  #+debug
  (place-descriptor ; contains the name of the computed variable, or the slot definition
   nil
   :type (or null symbol computed-effective-slot-definition))
  #+debug
  (form
   nil
   :type (or atom list)))

(def (function io) incf-pulse (computed-state)
  (declare (type computed-state computed-state)
           #+sbcl(sb-ext:muffle-conditions sb-ext:compiler-note))
  ;; TODO think about fixnum overflow: keeping pulse at fixnum range is good for performance
  (incf (pulse-of (cs-universe computed-state))))

(def (function io) current-pulse (computed-state)
  (declare (type computed-state computed-state)
           #+sbcl(sb-ext:muffle-conditions sb-ext:compiler-note))
  (pulse-of (cs-universe computed-state)))

(def (function io) copy-place-independent-slots-of-computed-state (from into)
  "Copy the slots of FROM into INTO that are not dependent on the place this computed slot has been assigned to."
  (declare (type computed-state from into))
  (macrolet ((x (&rest names)
               ;; due to inlining it may get expanded in a different package than *package*, so bind it explicitly
               (bind ((*package* (find-package :hu.dwim.computed-class)))
                 `(progn
                    ,@(loop for name :in names
                            for accessor = (symbolicate '#:cs- name)
                            collect `(setf (,accessor into) (,accessor from)))))))
    (x universe
       computed-at-pulse
       validated-at-pulse
       depends-on
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

(def (function ioe) computed-state-value (computed-state)
  "Read the value, recalculate when needed."
  (declare (type computed-state computed-state)
           #.(optimize-declaration))
  (%computed-state-value computed-state))

(def (function io) (setf computed-state-value) (new-value computed-state)
  "Set the value, invalidate and recalculate as needed."
  (declare (type computed-state computed-state)
           #.(optimize-declaration))
  (unless (eq (cs-kind computed-state) 'standalone)
    (error "You may only call (setf computed-state-value) on standalone computed-state's and ~A is not one" computed-state))
  (setf (%computed-state-value computed-state) new-value)
  (setf (cs-compute-as computed-state) nil))

(def (function io) %computed-state-value (computed-state)
  (declare (type computed-state computed-state)
           #.(optimize-declaration))
  (when (has-recompute-state-contex?)
    (bind ((context *recompute-state-contex*)
           (computed-state-being-recomputed (rsc-computed-state context)))
      (when (and (not (eq :always (cs-recomputation-mode computed-state-being-recomputed)))
                 (eq (cs-universe computed-state-being-recomputed)
                     (cs-universe computed-state)))
        (push computed-state (rsc-used-computed-states context)))))
  (ensure-computed-state-is-valid computed-state)
  (cs-value computed-state))

(def (function io) (setf %computed-state-value) (new-value computed-state)
  (declare (type computed-state computed-state)
           #.(optimize-declaration))
  (let ((current-pulse (incf-pulse computed-state)))
    ;; setting cs-depends-on to nil effectively makes it uncomputed, but other computed-state's can still depend on it
    ;; and get invalidated when the value is updated.
    (setf (cs-depends-on computed-state) nil)
    (setf (cs-computed-at-pulse computed-state) current-pulse)
    (setf (cs-validated-at-pulse computed-state) current-pulse)
    (setf (cs-value computed-state) new-value)))

(def (function ioe) computation-of-computed-state (computed-state)
  (declare (type computed-state computed-state)
           #.(optimize-declaration))
  (cs-compute-as computed-state))

(def (function io) (setf computation-of-computed-state) (new-value computed-state)
  (declare (type computed-state computed-state)
           (type function new-value)
           #.(optimize-declaration))
  (setf (cs-compute-as computed-state) new-value)
  (invalidate-computed-state computed-state #t))

(def function ensure-computed-state-is-valid (computed-state)
  (declare (type computed-state computed-state)
           #.(optimize-declaration))
  (multiple-value-bind (valid-p newer-computed-state)
      (computed-state-valid-p computed-state)
    (declare (ignorable newer-computed-state))
    (unless valid-p
      (log.dribble "About to recompute ~A because ~A is newer" computed-state newer-computed-state)
      (check-circularity computed-state)
      (recompute-computed-state computed-state)))
  (values))

(def (function e) recompute-computed-state (computed-state)
  (declare (type computed-state computed-state)
           #.(optimize-declaration))
  (with-new-recompute-state-contex (:computed-state computed-state)
    (bind ((context *recompute-state-contex*))
      (log.dribble "Recomputing slot ~A" computed-state)
      (bind ((old-value (cs-value computed-state))
             (new-value (aif (cs-compute-as computed-state)
                             (funcall it (cs-object computed-state) old-value)
                             (error "Now, this is bad: we encoutered an invalid computed-state ~A which is holding a constant and therefore has no compute-as lambda"
                                    computed-state)))
             (store-new-value-p (if (and (primitive-p old-value)
                                         (primitive-p new-value))
                                    (not (equal old-value new-value))
                                    (not (computed-value-equal? old-value new-value)))))
        (setf (cs-depends-on computed-state) (rsc-used-computed-states context))
        (when (or store-new-value-p
                  (= (cs-computed-at-pulse computed-state) #.+invalid-pulse+))
          (setf (cs-computed-at-pulse computed-state) (current-pulse computed-state))
          (setf (cs-validated-at-pulse computed-state) (current-pulse computed-state)))
        (if store-new-value-p
            (setf (cs-value computed-state) new-value)
            (log.dribble "Not storing fresh recomputed value for ~A because it was equal to the cached value." computed-state))
        new-value))))

(locally (declare #+sbcl(sb-ext:muffle-conditions sb-ext:compiler-note))
;; TODO: the muffle is only needed because pulse is not fixnum for now
(def function computed-state-valid-p (computed-state)
  (declare (type computed-state computed-state)
           #.(optimize-declaration))
  (if (eq :always (cs-recomputation-mode computed-state))
      (values #f computed-state)
      (let ((computed-at-pulse (cs-computed-at-pulse computed-state))
            (validated-at-pulse (cs-validated-at-pulse computed-state)))
        (log.dribble "Validating ~A" computed-state)
        (multiple-value-bind (valid-p newer-computed-state)
            (block valid-check
              (when (= (current-pulse computed-state) validated-at-pulse)
                (return-from valid-check (values #t nil)))
              (when (= computed-at-pulse #.+invalid-pulse+)
                (return-from valid-check (values #f computed-state)))
              (loop for depends-on-computed-state :in (cs-depends-on computed-state) do
                    (log.dribble "Comparing ~A to ~A" computed-state depends-on-computed-state)
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
                (log.dribble "Value turned out to be invalid for ~A" computed-state)
                (invalidate-computed-state computed-state #t)))
          (values valid-p newer-computed-state))))))

(def function check-circularity (computed-state)
  (declare (type computed-state computed-state)
           #.(optimize-declaration))
  (when (has-recompute-state-contex?)
    (bind ((context *recompute-state-contex*))
      (loop for parent-context = context :then (rsc-parent-context parent-context)
            while parent-context
            do (let ((parent-computed-state (rsc-computed-state parent-context)))
                 (when (eq computed-state parent-computed-state)
                   (error "Circularity detected among the computed slots/variables ~A"
                          (append (list computed-state)
                                  (loop for parent-context = context :then (rsc-parent-context parent-context)
                                        while parent-context
                                        collect (rsc-computed-state parent-context))))))))))

(def (function ioe) invalidate-computed-state (computed-state &optional locally)
  "Invalidate the given COMPUTED-STATE. When LOCALLY is #t then this invalidation has only local effects on this computed-state and the dependent computed-states are not invalidated."
  (declare (type computed-state computed-state))
  (unless locally
    (incf-pulse computed-state))
  (setf (cs-computed-at-pulse computed-state) #.+invalid-pulse+)
  (setf (cs-validated-at-pulse computed-state) #.+invalid-pulse+))

(def function print-computed-state (computed-state stream)
  (declare (type computed-state computed-state))
  #*((:debug
      (bind ((name (cs-place-descriptor computed-state)))
        (typecase name
          (computed-effective-slot-definition
           (setf name (slot-definition-name name))))
        (when (eq (cs-kind computed-state) 'object-slot)
          (format stream "~A / " (cs-object computed-state)))
        (format stream "<#~A :pulse ~A :value ~A :kind ~A>"
                name (cs-computed-at-pulse computed-state) (cs-value computed-state) (cs-kind computed-state))))
     (t
      (when (eq (cs-kind computed-state) 'object-slot)
        (format stream "~A / " (cs-object computed-state)))
      (format stream "<#computed-state :pulse ~A :value ~A :kind ~A>"
              (cs-computed-at-pulse computed-state) (cs-value computed-state) (cs-kind computed-state)))))

;;;;;;
;;; Helper methods

(def (function io) primitive-p (object)
  (or (numberp object)
      (stringp object)
      (symbolp object)
      (characterp object)))

(def (function io) computed-state-or-nil (object slot)
  (declare (type computed-object object)
           (type computed-effective-slot-definition slot)
           #.(optimize-declaration))
  (the (or null computed-state)
    (let ((result (standard-instance-access-form object slot)))
      (when (computed-state-p result)
        result))))

(def function expand-to-primitive-compute-as-form (input-form &optional env)
  (if (primitive-compute-as-form? input-form)
      input-form
      (loop with form = input-form
            with expanded-p = #t
            while (progn
                    (multiple-value-setq (form expanded-p) (macroexpand-1 form env))
                    expanded-p)
            do (when (primitive-compute-as-form? form)
                 (return form))
            finally (error "Form ~S can not be macroexpanded into a primitive-compute-as-form. This should not happen." input-form))))

(def function ensure-arguments-for-primitive-compute-as-form (form &rest args)
  (let ((form-args (copy-list (second form))))
    (loop for (indicator value) :on args :by #'cddr do
          (setf (getf form-args indicator) value))
    `(,(first form) ,form-args
      ,@(cddr form))))
