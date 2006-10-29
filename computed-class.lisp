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

(declaim #+optimize(inline incf-pulse current-pulse computed-state-object-slot-p
                           computed-state-value (setf computed-state-value) primitive-p
                           invalidate-computed-state computed-state-or-nil
                           standard-instance-access (setf standard-instance-access)))

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
  ()
  (:documentation "This is the base class for all computed classes. The class need not be listed in the direct classes when defining a computed class because the meta class takes care adding it."))

;;;;;;;;;;;;;;;;;;;
;;; Computed states

(defstruct (computed-universe (:conc-name cu-))
  "This counter will be incremented each time a computed slot is set either by calling slot-value or by the accessor. On the other hand when a computed slot is recomputed due to changes in the computed slots used when the original slot was last computed then this counter will not change. The first valid pulse value is 0."
  (pulse 0 :type integer)
  (name nil :type (or null string)))

(defparameter *default-universe* (make-computed-universe :name "Default computed universe"))

(defun incf-pulse (computed-state)
  (declare (inline) (type computed-state computed-state))
  (incf (cu-pulse (cs-universe computed-state))))

(defun current-pulse (computed-state)
  (declare (inline) (type computed-state computed-state))
  (cu-pulse (cs-universe computed-state)))

(eval-always
  (defconstant +invalid-pulse+ -1
    "The invalid pulse will be set in the computed-state whenever it has to be recomputed on the next read operation."))

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
           (inline)
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

;;;;;;;;;;;;;;;;;;;;
;;; CLOS MOP related

(defmethod validate-superclass ((class standard-class) (superclass computed-class))
  t)

(defmethod validate-superclass ((class computed-class) (superclass standard-class))
  t)

#+(or :generate-custom-reader :generate-custom-writer)
(defmethod finalize-inheritance :after ((class computed-class))
  (loop for direct-slot :in (class-direct-slots class)
        for effective-slot = (find-slot class (slot-definition-name direct-slot))
        when (typep effective-slot 'computed-effective-slot-definition) do
        (assert (and (<= (length (slot-definition-readers direct-slot)) 1)
                     (<= (length (slot-definition-writers direct-slot)) 1))
                () "Computed class does not support multiple readers and/or writers.")
        (let ((reader (first (slot-definition-readers direct-slot)))
              (writer (first (slot-definition-writers direct-slot))))
          #+generate-custom-reader
          (when reader
            ;; FIXME: this setf is a KLUDGE to stop sbcl from generating it's own reader after (?!) this
            (setf (slot-definition-readers direct-slot) nil)
            (let ((reader-gf (ensure-generic-function reader))) 
              (ensure-method reader-gf
                             `(lambda (object)
                               ,(slot-value-using-class-body class effective-slot))
                             :specializers (list class))))
          #+generate-custom-writer
          (when writer
            ;; FIXME: this setf is a KLUDGE to stop sbcl from generating it's own writer after (?!) this
            (setf (slot-definition-writers direct-slot) nil)
            (ensure-generic-function writer)
            (let ((writer-gf (ensure-generic-function writer))) 
              (ensure-method writer-gf
                             `(lambda (new-value object)
                               ,(setf-slot-value-using-class-body class effective-slot))
                             :specializers (list (find-class 't) class)))))))

(defmethod direct-slot-definition-class ((class computed-class) &key initform (computed #f) &allow-other-keys)
  (if (and (not computed)
           (or (not (consp initform))
               (not (symbolp (first initform)))
               (not (get (first initform) 'computed-as-macro-p))))
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

(eval-always
  ;; the purpose of this is to be able to share the code between the accessors and the real svuc.
  ;; (iow, to avoid fatal copy-paste errors)
  ;; we evaluate some computations at call time when we have the information
  (defun slot-value-using-class-body (&optional class slot)
    `(let ((computed-state ,(if (and class slot)
                                ;; TODO this slot-boundp-using-class is probably expensive here compared to the rest
                                `(when (slot-boundp-using-class ,class object ,slot)
                                  ,(standard-instance-access-form 'object (slot-definition-location slot)))
                                `(computed-state-or-nil class object slot))))
      (if computed-state
          (progn
            (when (has-recompute-state-contex)
              (in-recompute-state-contex context
                (push computed-state (rsc-used-computed-states context))))
            (computed-state-value computed-state))
          ,(if slot
               (standard-instance-access-form 'object (slot-definition-location slot))
               (standard-instance-access-form 'object '(slot-definition-location slot))))))

  (defun setf-slot-value-using-class-body (&optional class slot)
    `(progn
      #+debug(when ,(if (and class slot)
                        `(slot-boundp-using-class ,class object ,slot)
                        `(slot-boundp-using-class class object slot))
               (let ((old-computed-state ,(if slot
                                              (standard-instance-access-form 'object (slot-definition-location slot))
                                              (standard-instance-access-form 'object '(slot-definition-location slot)))))
                 (when (computed-state-p old-computed-state)
                   (setf (cs-attached-to-object-p old-computed-state) #f))))
      (if (typep new-value 'computed-state)
          (let ((new-computed-state new-value))
            #+debug(setf (cs-attached-to-object-p new-computed-state) #t)
            (setf (cs-object new-computed-state) object)
            (setf (cs-slot new-computed-state) ,(or slot 'slot))
            (invalidate-computed-state new-computed-state)
            (incf-pulse new-computed-state)
            ,(if slot
                 (setf-standard-instance-access-form 'new-computed-state 'object (slot-definition-location slot))
                 (setf-standard-instance-access-form 'new-computed-state 'object '(slot-definition-location slot))))
          (let ((computed-state (computed-state-or-nil ,(or class 'class) object ,(or slot 'slot))))
            (if computed-state
                (setf (computed-state-value computed-state) new-value)
                ,(if slot
                     (setf-standard-instance-access-form 'new-value 'object (slot-definition-location slot))
                     (setf-standard-instance-access-form 'new-value 'object '(slot-definition-location slot))))))
      new-value)))

(defmethod slot-value-using-class ((class computed-class)
                                   (object computed-object)
                                   (slot computed-effective-slot-definition))
  (declare #.(optimize-declaration))
  #.(slot-value-using-class-body))

(defmethod (setf slot-value-using-class) (new-value
                                          (class computed-class)
                                          (object computed-object)
                                          (slot computed-effective-slot-definition))
  (declare #.(optimize-declaration))
  #.(setf-slot-value-using-class-body))

(defun computed-state-value (computed-state)
  "Read the value, recalculate when needed."
  (declare (type computed-state computed-state)
           (inline)
           #.(optimize-declaration))
  (ensure-computed-state-is-valid computed-state)
  (cs-value computed-state))

(defun (setf computed-state-value) (new-value computed-state)
  "Set the value, invalidate and recalculate as needed."
  (declare (type computed-state computed-state)
           (inline)
           #.(optimize-declaration))
  (let ((current-pulse (incf-pulse computed-state))) 
    (setf (cs-depends-on computed-state) nil)
    (setf (cs-computed-at-pulse computed-state) current-pulse)
    (setf (cs-validated-at-pulse computed-state) current-pulse)
    (setf (cs-value computed-state) new-value)))

;; TODO this is broken on clisp, we don't even get here when there's already an error signalled
(defmethod shared-initialize :around ((class computed-class) slot-names &rest args
                                      &key direct-superclasses direct-slots &allow-other-keys)
  "Support :computed #f slot argument for documentation purposes."
  (remf-keywords args :direct-superclasses :direct-slots)
  (let* ((computed-object (find-class 'computed-object))
         (direct-superclasses (if (member computed-object direct-superclasses :test 'eq)
                                  direct-superclasses
                                  (append direct-superclasses (list computed-object))))
         (direct-slots (loop for direct-slot :in direct-slots
                             collect (progn
                                       (unless (getf direct-slot :computed)
                                         (remf-keywords direct-slot :computed))
                                       direct-slot))))
    (apply #'call-next-method class slot-names :direct-superclasses direct-superclasses :direct-slots direct-slots args)))

;; this definition is here to allow a :computed argument for the slot definition
(defmethod shared-initialize :before ((object computed-slot-definition) slot-names &key computed &allow-other-keys)
  (declare (ignore object slot-names computed)))

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
           (type symbol slot-name)
           #.(optimize-declaration)
           (inline))
  (find slot-name (the list (class-slots class)) :key #'slot-definition-name))

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
                                                                 (computed-state-or-nil (class-of used-object) used-object used-slot)
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
  (declare (type computed-state computed-state)
           (inline))
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
        (format stream "<#~A :pulse ~A>" slot-name (cs-computed-at-pulse computed-state))
        (format stream "~A/<#~A :pulse ~A :attached ~A>"
                object slot-name (cs-computed-at-pulse computed-state) attached-p))))

;;;;;;;;;;;;;;;;;;;;
;;; Public interface

(defmacro define-computed-universe (compute-as-macro-name &key (name (let ((*package* (find-package "KEYWORD")))
                                                                       (format nil "~S" compute-as-macro-name))))
  "Use define-computed-universe to define a universe glueing together computed slots. It will define a macro with the given name that can be used to initialize computed slots with a computation."
  ;; mark on the symbol that this is a compute-as macro
  (declare (type symbol compute-as-macro-name))
  (let ((verbose-compute-as-macro-name (concatenate-symbol compute-as-macro-name "*"))
        (docstring (strcat "Use this macro to set the value of a computed slot to a computation in the universe '" (string name) "'.")))
    `(eval-when (:compile-toplevel :load-toplevel)
      (setf (get ',compute-as-macro-name 'computed-as-macro-p) t)
      (unless (get ',compute-as-macro-name 'computed-universe)
        (setf (get ',compute-as-macro-name 'computed-universe) (make-computed-universe :name ,name)))
      (defmacro ,verbose-compute-as-macro-name ((&key slot) &body form)
        ,docstring
        `(make-computed-state :universe (get ',',compute-as-macro-name 'computed-universe)
          #+debug :form #+debug ',form
          :compute-as (lambda (self current-value)
                        (declare (ignorable self current-value))
                        ,@form)
          ,@(when slot
              (list :slot slot))))
      (defmacro ,compute-as-macro-name (&body body)
        ,docstring
        `(,',verbose-compute-as-macro-name ()
          ,@body)))))

(defgeneric computed-value-equal-p (old-value new-value)
  (:documentation "When a new value is set in a computed slot, then this method is used to decide whether dependent slots should be recalculated or not.")
  (:method (old-value new-value)
           #f))

(defgeneric invalidate-computed-slot (object slot)
  (:documentation "Forces the recalculation of a slot on the next slot-value or accessor call.")
  (:method ((object computed-object) (slot-name symbol))
           (invalidate-computed-slot object (find-slot (class-of object) slot-name)))
  (:method ((object computed-object) (slot computed-effective-slot-definition))
           (let ((computed-state (computed-state-or-nil (class-of object) object slot)))
             (if computed-state
                 (invalidate-computed-state computed-state)
                 (error "The slot ~A of ~A is not computed while invalidate-computed-slot was called on it" slot object)))))

(defgeneric make-slot-uncomputed (object slot)
  (:documentation "Makes the slot a constant slot with respect to other computed slots.")
  (:method ((object computed-object) (slot-name symbol))
           (make-slot-uncomputed object (find-slot (class-of object) slot-name)))
  (:method ((object computed-object) (slot computed-effective-slot-definition))
           (let* ((class (class-of object))
                  (computed-state (computed-state-or-nil class object slot)))
             (when computed-state
               ;; first we must make it unbound, so the (setf slot-value-using-class) will simply (call-next-method) with the new value
               (slot-makunbound-using-class class object slot)
               (setf (slot-value-using-class class object slot) (cs-value computed-state))))))

(defgeneric recompute-slot (object slot)
  (:documentation "Enforces the recomputation of the given slot")
  (:method ((object computed-object) (slot-name symbol))
           (recompute-slot object (find-slot (class-of object) slot-name)))
  (:method ((object computed-object) (slot computed-effective-slot-definition))
           (let ((computed-state (computed-state-or-nil (class-of object) object slot)))
             (if computed-state
                 (recompute-computed-state computed-state)
                 (error "The slot ~A of ~A is not computed while recompute-slot was called on it" slot object)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Managing computed slot state 

(defun computed-state-or-nil (class object slot)
  (declare (type (or symbol computed-object) object)
           (type (or null computed-effective-slot-definition) slot)
           #.(optimize-declaration)
           (inline))
  (the (or null computed-state)
    (when (and (not (eq object 'not-an-object-slot-state))
               (slot-boundp-using-class class object slot))
      (let ((result #.(standard-instance-access-form 'object '(slot-definition-location slot))))
        (when (computed-state-p result)
            result)))))

;;;;;;;;;;;;;;;;;;;;;;;;
;;; Standalone variables

;; limitations:
;; - computed-let can only handle variables initialized with (compute-as ...) forms (can be fixed)
;; - (computed-state-for NAME) works only on one level (probably not trivial to overcome this)
;; - warnings/notes due to unused functions and optimization

(define-setf-expander computed-state-in-variable-value ())

(defmacro clet (vars &body body)
  "A let* with extra semantics to handle computed variables. For now see the code and the test file for details.
   Available bindings in the body:
     - (computed-state-for NAME) A macro to access the place itself, so you can setf closed-over computed variables to new (compute-as ...) forms."
  (let ((state-variables (loop for (name nil) :in vars
                               collect (gensym (string name)))))
    ;; wrap the global computed-state-value accessors and do some extra work specific to handling variables
    `(flet ((computed-state-value (computed-state)
             (declare #.(optimize-declaration))
             (when (has-recompute-state-contex)
               (in-recompute-state-contex context
                 (push computed-state (rsc-used-computed-states context))))
             (computed-state-value computed-state))
           ((setf computed-state-value) (new-value computed-state)
             (declare #.(optimize-declaration))
             #+debug(assert (not (typep new-value 'computed-state)) () "Setting computed-state's into variables should be done through the (computed-state-for var-name) form")
             (setf (computed-state-value computed-state) new-value)))
      (symbol-macrolet (,@(loop for (name definition) :in vars
                                for var = (gensym (string name))
                                collect var :into vars
                                collect (list name `(computed-state-value ,var)) :into result
                                finally (progn
                                          (setf state-variables vars)
                                          (return result)))
                          ;; these are NAME-state definitions that expand to the gensym-ed variables,
                          ;; so through them you can access the actual states directly (e.g. to set
                          ;; state variables captured by various closures) 
                          ,@(loop for (name definition) :in vars
                                  for var :in state-variables
                                  for state-name = (concatenate-symbol name "-state")
                                  collect (list state-name var)))
        (macrolet ((computed-state-for (variable)
                     (case variable
                       ,@(loop for (name nil) :in vars
                               for state-variable :in state-variables
                               collect `(,name ',state-variable))
                       (t (error "Limitation: computed-state-for can only access the state variables in the closest computed-let form. Variable ~S was not found there." variable)))))
          (let* ,(loop for (name definition) :in vars
                       for var :in state-variables
                       collect (list var definition))
            (declare (ignorable ,@state-variables))
            ,@body))))))



