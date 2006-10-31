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



;;;;;;;;;;;;;;;;;;;;
;;; CLOS MOP related

(defclass computed-class (standard-class)
  ()
  (:documentation "A computed class might have slots which are computed based on other computed slots in other computed class instances. A slot of a computed class is either a standard slot or a computed slot and only class redefinition may change this. Slots which are computed will be tracked, invalidated and/or recomputed whenever a computed slot value changes which were used last time when the slot was computed. The used computed slots are collected runtime and per instance. Moreover different instances might compute the same slots in different ways."))

(defclass computed-slot-definition (standard-slot-definition)
  ((compute-as
    :type computed-state
    :accessor compute-as-of
    :initarg :compute-as)
   (computed-readers
    :type list
    :accessor computed-readers-of
    :initarg :computed-readers)
   (computed-writers
    :type list
    :accessor computed-writers-of
    :initarg :computed-writers)))

(defclass computed-direct-slot-definition (computed-slot-definition standard-direct-slot-definition)
  ())

(defclass computed-effective-slot-definition (computed-slot-definition standard-effective-slot-definition)
  ())

(defclass computed-object ()
  ()
  (:documentation "This is the base class for all computed classes. The class need not be listed in the direct classes when defining a computed class because the meta class takes care adding it."))

(defmethod validate-superclass ((class standard-class) (superclass computed-class))
  t)
(defmethod validate-superclass ((class computed-class) (superclass standard-class))
  t)

(defmethod initialize-instance :around ((slot computed-direct-slot-definition) &rest args &key readers writers &allow-other-keys)
  (remf-keywords args :readers :writers)
  ;; convert :readers to :computed-readers and the same with :writer, so the underlying MOP won't generate its own accessors
  (apply #'call-next-method slot :computed-readers readers :computed-writers writers args))

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
    (aprog1
        (call-next-method)
      ;; We copy the computed-readers and computed-writers slots to the effective-slot, so we can
      ;; access it later when generating custom accessors.
      (when (typep it 'computed-effective-slot-definition)
        (setf (computed-readers-of it)
              (remove-duplicates (loop for direct-slot-definition :in direct-slot-definitions
                                       appending (if (typep direct-slot-definition 'computed-direct-slot-definition)
                                                     (computed-readers-of direct-slot-definition)
                                                     (slot-definition-readers direct-slot-definition)))
                                 :test #'equal))
        (setf (computed-writers-of it)
              (remove-duplicates (loop for direct-slot-definition :in direct-slot-definitions
                                       appending (if (typep direct-slot-definition 'computed-direct-slot-definition)
                                                     (computed-writers-of direct-slot-definition)
                                                     (slot-definition-writers direct-slot-definition)))
                                 :test #'equal))))))
(eval-always
  ;; These are called at read-time and the purpose of this is to be able to share the code between
  ;; the accessors and the real svuc (IOW, to avoid fatal copy-paste errors) and to be able to
  ;; evaluate some computations at compule time when some more information is available (when
  ;; generating accessors (in finalize-inheritance :after) the slot-definition-location can be
  ;; captured into the generated accessors)
  (defun slot-value-using-class-body (&optional slot)
    `(let ((slot-value ,(standard-instance-access-form slot)))
      (when (eq slot-value ',+unbound-slot-value+)
        (error 'unbound-slot
               :name ',(if slot
                          (slot-definition-name slot)
                          '(slot-definition-name slot))
               :instance object))
      (if (computed-state-p slot-value)
          (let ((computed-state slot-value))
            (when (has-recompute-state-contex)
              (in-recompute-state-contex context
                (push computed-state (rsc-used-computed-states context))))
            (computed-state-value computed-state))
          slot-value)))

  (defun setf-slot-value-using-class-body (&optional slot)
    `(let ((slot-value ,(standard-instance-access-form slot)))
      (if (computed-state-p new-value)
          (progn
            #+debug(progn
                     (when (computed-state-p slot-value)
                       (setf (cs-attached-to-object-p slot-value) #f))
                     (setf (cs-attached-to-object-p new-value) #t))
            (setf (cs-object new-value) object)
            (setf (cs-slot new-value) ,(or slot 'slot))
            (invalidate-computed-state new-value)
            (incf-pulse new-value)
            ,(setf-standard-instance-access-form slot))
          (if (computed-state-p slot-value)
              (setf (computed-state-value slot-value) new-value)
              ,(setf-standard-instance-access-form slot)))
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

(defmethod slot-boundp-using-class ((class computed-class)
                                    (object computed-object)
                                    (slot computed-effective-slot-definition))
  (declare #.(optimize-declaration))
  (not (eq #.(standard-instance-access-form)
           '#.+unbound-slot-value+)))

(defmethod slot-makunbound-using-class ((class computed-class)
                                        (object computed-object)
                                        (slot computed-effective-slot-definition))
  (declare #.(optimize-declaration))
  #.(setf-standard-instance-access-form nil (quote (quote #.+unbound-slot-value+))))

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

(defclass computed-accessor-method (standard-accessor-method)
  ((effective-slot
    :initarg :effective-slot
    :accessor effective-slot-of
    :documentation "This method was generatated or validated using this effective slot object."))
  (:documentation "computed-class generates method with this class."))

(defclass computed-reader-method (computed-accessor-method standard-reader-method)
  ())

(defclass computed-writer-method (computed-accessor-method standard-writer-method)
  ())

#+debug
(progn
  (defparameter *kept-accessors* 0)
  (defparameter *new-accessors* 0))

(defun ensure-accessor-for (class accessor-name effective-slot type)
  (let* ((gf (ensure-generic-function accessor-name :lambda-list (ecase type
                                                                   (:reader '(object))
                                                                   (:writer '(new-value object)))))
         (specializers (ecase type
                         (:reader (list class))
                         (:writer (list (find-class 't) class))))
         (current-method (find-method gf '() specializers #f)))
    (if (and current-method
             (typep current-method 'computed-accessor-method)
             (= (slot-definition-location (effective-slot-of current-method))
                (slot-definition-location effective-slot)))
        (progn
          (log.dribble "Keeping compatible ~A for class ~A, slot ~S, slot-location ~A"
                       (string-downcase (symbol-name type)) class (slot-definition-name effective-slot)
                       (slot-definition-location effective-slot))
          #+debug(incf *kept-accessors*)
          (setf (effective-slot-of current-method) effective-slot))
        (progn
          (log.debug "Ensuring new ~A for class ~A, slot ~S, effective-slot ~A, slot-location ~A"
                     (string-downcase (symbol-name type)) class (slot-definition-name effective-slot)
                     effective-slot (slot-definition-location effective-slot))          
          #+debug(incf *new-accessors*)
          (setf (effective-slot-of
                 (ensure-method gf
                                (ecase type
                                  (:reader
                                   `(lambda (object)
                                     (declare (optimize (speed 1)))
                                     (log.dribble "Entered reader for object ~A, generated for class ~A, slot ~A, slot-location ~A"
                                      object ,class ,effective-slot ,(slot-definition-location effective-slot))
                                     (if (eq (class-of object) ,class)
                                         (progn
                                           ,(slot-value-using-class-body effective-slot))
                                         (progn
                                           (log.dribble "Falling back to slot-value in reader for object ~A, slot ~A"
                                                        object (slot-definition-name ,effective-slot))
                                           (slot-value object ',(slot-definition-name effective-slot))))))
                                  (:writer
                                   `(lambda (new-value object)
                                     (declare (optimize (speed 1)))
                                     (log.dribble "Entered writer for object ~A, generated for class ~A, slot ~A, slot-location ~A"
                                      object ,class ,effective-slot ,(slot-definition-location effective-slot))
                                     (if (eq (class-of object) ,class)
                                         (progn
                                           ,(setf-slot-value-using-class-body effective-slot))
                                         (progn
                                           (log.dribble "Falling back to (setf slot-value) in writer for object ~A, slot ~A"
                                                        object  (slot-definition-name ,effective-slot))
                                           (setf (slot-value object ',(slot-definition-name effective-slot)) new-value))))))
                                :specializers specializers
                                :method-class (find-class 'computed-reader-method)))
                effective-slot)))))

(defun ensure-accessors-for (class)
  (loop for effective-slot :in (class-slots class)
        when (typep effective-slot 'computed-effective-slot-definition) do
        (log.dribble "Visiting effective-slot ~A of class ~A to generate accessors" effective-slot class)
        #+generate-custom-readers
        (dolist (reader (computed-readers-of effective-slot))
          (ensure-accessor-for class reader effective-slot :reader))
        #+generate-custom-writers
        (dolist (writer (computed-writers-of effective-slot))
          (ensure-accessor-for class writer effective-slot :writer))))

#+(or generate-custom-readers generate-custom-writers)
(defmethod finalize-inheritance :after ((class computed-class))
  (ensure-accessors-for class))

;; TODO this is not standard compliant: "Portable programs must not define methods on
;; shared-initialize." (for classes)
(defmethod shared-initialize :around ((class computed-class) slot-names &rest args
                                      &key direct-superclasses direct-slots &allow-other-keys)
  "Support :computed #f slot argument for documentation purposes."
  (remf-keywords args :direct-superclasses :direct-slots)
  (let* ((computed-object (find-class 'computed-object))
         (direct-superclasses (if (member computed-object direct-superclasses :test 'eq)
                                  direct-superclasses
                                  (append direct-superclasses (list computed-object))))
         ;; TODO this has no effect on clisp: we don't even get here when there's already an error signalled for :computed #f
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
;;; Implementation

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
           (let ((computed-state (computed-state-or-nil object slot)))
             (if computed-state
                 (invalidate-computed-state computed-state)
                 (error "The slot ~A of ~A is not computed while invalidate-computed-slot was called on it" slot object)))))

(defgeneric make-slot-uncomputed (object slot)
  (:documentation "Makes the slot a constant slot with respect to other computed slots.")
  (:method ((object computed-object) (slot-name symbol))
           (make-slot-uncomputed object (find-slot (class-of object) slot-name)))
  (:method ((object computed-object) (slot computed-effective-slot-definition))
           (let* ((class (class-of object))
                  (computed-state (computed-state-or-nil object slot)))
             (when computed-state
               ;; first we must make it unbound, so the (setf slot-value-using-class) will simply (call-next-method) with the new value
               (slot-makunbound-using-class class object slot)
               (setf (slot-value-using-class class object slot) (cs-value computed-state))))))

(defgeneric recompute-slot (object slot)
  (:documentation "Enforces the recomputation of the given slot")
  (:method ((object computed-object) (slot-name symbol))
           (recompute-slot object (find-slot (class-of object) slot-name)))
  (:method ((object computed-object) (slot computed-effective-slot-definition))
           (let ((computed-state (computed-state-or-nil object slot)))
             (if computed-state
                 (recompute-computed-state computed-state)
                 (error "The slot ~A of ~A is not computed while recompute-slot was called on it" slot object)))))




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


