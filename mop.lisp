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
;;; CLOS MOP related

(defclass computed-class (standard-class)
  ()
  (:documentation "A computed class might have slots which are computed based on other computed slots in other computed class instances. A slot of a computed class is either a standard slot or a computed slot and only class redefinition may change this. Slots which are computed will be tracked, invalidated and/or recomputed whenever a computed slot value changes which were used last time when the slot was computed. The used computed slots are collected runtime and per instance. Moreover different instances might compute the same slots in different ways."))

(defclass computed-class* (computed-class)
  ()
  (:documentation "Just like computed-class but the classes having this metaclass will have custom accessors. This slows down loading but speeds up the accessors quite a bit."))

(defmethod validate-superclass ((class standard-class) (superclass computed-class))
  t)
(defmethod validate-superclass ((class computed-class) (superclass standard-class))
  t)

(defclass computed-object ()
  ()
  (:documentation "This is the base class for all computed classes. The class need not be listed in the direct supers when defining a computed class because the metaclass makes sure it's among them."))

(defclass computed-slot-definition (standard-slot-definition)
  ((computed-in
    :initform nil
    :type symbol
    :accessor computed-in-of
    :initarg :computed-in)
   (computed-readers
    :initform nil
    :type list
    :accessor computed-readers-of
    :initarg :computed-readers)
   (computed-writers
    :initform nil
    :type list
    :accessor computed-writers-of
    :initarg :computed-writers)))

(defclass computed-direct-slot-definition (computed-slot-definition standard-direct-slot-definition)
  ())

(defclass computed-direct-slot-definition-with-custom-accessors (computed-direct-slot-definition)
  ()
  (:documentation "This direct slot definition converts the :readers and :writers initargs to :computed-readers and :computed-writers effectively disabling the generation of default accessors."))

(defclass computed-effective-slot-definition (computed-slot-definition standard-effective-slot-definition)
  ())

(defmethod shared-initialize :around ((computed-slot-definition computed-direct-slot-definition) slot-names
                                      &rest args &key (initform nil initform-p) computed-in &allow-other-keys)
  ;; convert the initform into a compute-as* primitive form, assert and set computed-in properly
  (when computed-in
    (assert (get computed-in 'computed-as-macro-p) ()
            "The specified :computed-in argument ~S is not a compute-as macro in any computed universe"
            computed-in)
    (setf computed-in (get computed-in 'primitive-compute-as-macro))
    (assert computed-in))
  (let ((primitive-form (if (compute-as-form-p initform)
                            (primitive-compute-as-form-of initform)
                            initform)))
    (when (and initform-p
               primitive-form)
      (if computed-in
          (assert (eq (first primitive-form) computed-in) ()
                  ":computed-in and the :initform parameters are not consistent. ~S is not computed in ~S"
                  initform computed-in)
          (setf computed-in (first primitive-form)))
      ;; to enforce :kind 'object-slot. should it be an assert instead?
      ;;(setf primitive-form (ensure-arguments-for-primitive-compute-as-form primitive-form :kind 'object-slot))
      )
    (apply #'call-next-method computed-slot-definition slot-names
           (append
            (list :computed-in computed-in)
            (when initform-p
              (list :initform primitive-form
                    :initfunction (if primitive-form
                                      (compile nil `(lambda () ,primitive-form))
                                      (constantly nil))))
            args))))

(defmethod initialize-instance :around ((slot computed-direct-slot-definition-with-custom-accessors)
                                        &rest args &key readers writers &allow-other-keys)
  (remf-keywords args :readers :writers)
  (apply #'call-next-method slot :computed-readers readers :computed-writers writers args))

(defun needs-to-be-computed-direct-slot-p (slot-initargs)
  (let ((initform (getf slot-initargs :initform)))
    (or (getf slot-initargs :computed-in)
        (compute-as-form-p initform))))

(defmethod direct-slot-definition-class ((class computed-class) &rest slot-initargs)
  (if (needs-to-be-computed-direct-slot-p slot-initargs)
      (find-class 'computed-direct-slot-definition)
      (call-next-method)))

(defmethod direct-slot-definition-class ((class computed-class*) &key initform (computed-in #f) &allow-other-keys)
  (if (or computed-in
          (compute-as-form-p initform))
      (find-class 'computed-direct-slot-definition-with-custom-accessors)
      (call-next-method)))

(defmethod effective-slot-definition-class ((class computed-class) &key &allow-other-keys)
  (declare (special %effective-slot-definition-class%))
  (aif %effective-slot-definition-class%
       (find-class it)
       (call-next-method)))

(defun needs-to-be-computed-effective-slot-p (direct-slot-definitions)
  (find-if (lambda (direct-slot-definition)
             (typep direct-slot-definition 'computed-direct-slot-definition))
           direct-slot-definitions))

(defmethod compute-effective-slot-definition :around ((class computed-class) name direct-slot-definitions)
  (declare (type list direct-slot-definitions))
  ;; TODO: it is unclear what to do when the direct slot definitions have different computed-in specifications
  (let ((%effective-slot-definition-class% (when (needs-to-be-computed-effective-slot-p direct-slot-definitions)
                                             'computed-effective-slot-definition)))
    (declare (special %effective-slot-definition-class%))
    (aprog1
        (call-next-method)
      ;; We collect and copy the readers and writers to the effective-slot, so we can access it
      ;; later when generating custom accessors.
      (when (typep it 'computed-effective-slot-definition)
        (setf (computed-in-of it) (some (lambda (slot) (when (typep slot 'computed-direct-slot-definition)
                                                         (computed-in-of slot)))
                                        direct-slot-definitions))
        (assert (computed-in-of it) nil "Computed effective slots must be assigned to a computed universe")
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
                                 :test #'equal))
        (when (typep it 'computed-direct-slot-definition-with-custom-accessors)
          ;; ensure the generic functions early, so we avoid compile time warnings of undefined functions
          (map nil 'ensure-generic-function-for-accessor (computed-readers-of it))
          (map nil 'ensure-generic-function-for-accessor (computed-writers-of it)))))))

(defmacro slot-value-using-class-body (object slot)
  (declare (type (or symbol effective-slot-definition) slot))
  `(let ((slot-value (standard-instance-access-form ,object ,slot)))
    (when (eq slot-value ',+unbound-slot-value+)
      (error 'unbound-slot
             :name ,(if (symbolp slot)
                        `(slot-definition-name ',slot)
                        `(quote ,(slot-definition-name slot)))
             :instance ,object))
    (if (computed-state-p slot-value)
        (%computed-state-value slot-value)
        slot-value)))

(defmacro setf-slot-value-using-class-body (new-value object slot)
  (declare (type (or symbol effective-slot-definition) slot))
  `(let ((slot-value (standard-instance-access-form ,object ,slot)))
    ;; an equivalent cond is compiled into considerably slower code on sbcl (?!).
    (if (computed-state-p ,new-value)
        (progn
          (unless (eq (cs-kind ,new-value) 'object-slot)
            (error "Trying to set the computed-state ~A into an object slot (wrong kind)" ,new-value))
          (if (computed-state-p slot-value)
              (copy-place-independent-slots-of-computed-state ,new-value slot-value)
              (progn
                (setf slot-value new-value)
                (setf (cs-object ,new-value) ,object)
                (setf (cs-slot ,new-value) ,slot)
                (setf-standard-instance-access-form slot-value ,object ,slot)))
          (invalidate-computed-state slot-value)
          slot-value)
        (if (computed-state-p slot-value)
            (setf (%computed-state-value slot-value) ,new-value)
            ;; by default unbound computed slots are initialized to be a computed slot, even when setting a constant in them.
            (if (eq slot-value (load-time-value +unbound-slot-value+))
                (setf-standard-instance-access-form (make-computed-state :universe
                                                                         ,(if (symbolp slot)
                                                                              `(get (computed-in-of ,slot) 'computed-universe)
                                                                              `(get ',(computed-in-of slot) 'computed-universe))
                                                                         #+debug :form #+debug ,new-value
                                                                         :compute-as (constantly ,new-value)
                                                                         :kind 'object-slot
                                                                         :object ,object
                                                                         :slot ,slot)
                                                    ,object
                                                    ,slot)
                ;; there was a non-computed-state in the slot and we are setting a non-computed-state
                ;; new-value: keep the slot uncomputed.
                (setf-standard-instance-access-form ,new-value ,object ,slot))))))

(defmethod slot-value-using-class ((class computed-class)
                                   (object computed-object)
                                   (slot computed-effective-slot-definition))
  (declare #.(optimize-declaration))
  (slot-value-using-class-body object slot))

(defmethod (setf slot-value-using-class) (new-value
                                          (class computed-class)
                                          (object computed-object)
                                          (slot computed-effective-slot-definition))
  (declare #.(optimize-declaration))
  (setf-slot-value-using-class-body new-value object slot))

(defmethod slot-boundp-using-class ((class computed-class)
                                    (object computed-object)
                                    (slot computed-effective-slot-definition))
  (declare #.(optimize-declaration))
  (not (eq (standard-instance-access-form object slot)
           (load-time-value +unbound-slot-value+))))

(defmethod slot-makunbound-using-class ((class computed-class)
                                        (object computed-object)
                                        (slot computed-effective-slot-definition))
  (declare #.(optimize-declaration))
  (setf-standard-instance-access-form (load-time-value +unbound-slot-value+) object slot))

(defclass computed-accessor-method (standard-accessor-method)
  ((effective-slot
    :initarg :effective-slot
    :accessor effective-slot-of
    :documentation "This method was generatated or validated using this effective slot object."))
  (:documentation "computed-class generates accessors with this class."))

(defclass computed-reader-method (computed-accessor-method standard-reader-method)
  ())

(defclass computed-writer-method (computed-accessor-method standard-writer-method)
  ())

#+debug
(progn
  (defparameter *kept-accessors* 0)
  (defparameter *new-accessors* 0))

(defun ensure-generic-function-for-accessor (accessor-name type)
  (ensure-generic-function accessor-name :lambda-list (ecase type
                                                        (:reader '(object))
                                                        (:writer '(new-value object)))))

(defun ensure-accessor-for (class accessor-name effective-slot type)
  (let* ((gf (ensure-generic-function-for-accessor accessor-name type))
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
          (let  ((method (ensure-method gf
                                        (ecase type
                                          (:reader
                                           `(lambda (object)
                                             (declare (optimize (speed 1))) ; (speed 1) to ignore compiler notes when defining accessors
                                             (log.dribble "Entered reader for object ~A, generated for class ~A, slot ~A, slot-location ~A"
                                              object ,class ,effective-slot ,(slot-definition-location effective-slot))
                                             (if (eq (class-of object) ,class)
                                                 (progn
                                                   ,(macroexpand `(slot-value-using-class-body object ,effective-slot)))
                                                 (progn
                                                   (log.dribble "Falling back to slot-value in reader for object ~A, slot ~A"
                                                                object (slot-definition-name ,effective-slot))
                                                   (slot-value object ',(slot-definition-name effective-slot))))))
                                          (:writer
                                           `(lambda (new-value object)
                                             (declare (optimize (speed 1))) ; (speed 1) to ignore compiler notes when defining accessors
                                             (log.dribble "Entered writer for object ~A, generated for class ~A, slot ~A, slot-location ~A"
                                              object ,class ,effective-slot ,(slot-definition-location effective-slot))
                                             (if (eq (class-of object) ,class)
                                                 (progn
                                                   ,(macroexpand `(setf-slot-value-using-class-body new-value object ,effective-slot)))
                                                 (progn
                                                   (log.dribble "Falling back to (setf slot-value) in writer for object ~A, slot ~A"
                                                                object  (slot-definition-name ,effective-slot))
                                                   (setf (slot-value object ',(slot-definition-name effective-slot)) new-value))))))
                                        :specializers specializers
                                        #+ensure-method-supports-method-class :method-class
                                        #+ensure-method-supports-method-class (find-class 'computed-reader-method))))
            (declare (ignorable method))
            #+ensure-method-supports-method-class
            (setf (effective-slot-of method) effective-slot))))))

(defun ensure-accessors-for (class)
  (loop for effective-slot :in (class-slots class)
        when (typep effective-slot 'computed-effective-slot-definition) do
        (log.dribble "Visiting effective-slot ~A of class ~A to generate accessors" effective-slot class)
        (dolist (reader (computed-readers-of effective-slot))
          (ensure-accessor-for class reader effective-slot :reader))
        (dolist (writer (computed-writers-of effective-slot))
          (ensure-accessor-for class writer effective-slot :writer))))

(defmethod finalize-inheritance :after ((class computed-class*))
  (ensure-accessors-for class))

;;; make sure computed-object is among the supers (thanks to Pascal Constanza)
(defmethod initialize-instance :around ((class computed-class) &rest initargs &key direct-superclasses)
  (declare (dynamic-extent initargs))
  (if (loop for class in direct-superclasses
            thereis (ignore-errors (subtypep class (find-class 'computed-object))))
      (call-next-method)
      (apply #'call-next-method
             class
             :direct-superclasses (append direct-superclasses (list (find-class 'computed-object)))
             initargs)))

(defmethod reinitialize-instance :around ((class computed-class) &rest initargs
                                          &key (direct-superclasses '() direct-superclasses-p))
  (declare (dynamic-extent initargs))
  (if direct-superclasses-p
      ;; if direct superclasses are explicitly passed
      ;; this is exactly like above
      (if (loop for class in direct-superclasses
                thereis (ignore-errors (subtypep class (find-class 'computed-object))))
          (call-next-method)
          (apply #'call-next-method
                 class
                 :direct-superclasses (append direct-superclasses (list (find-class 'computed-object)))
                 initargs))
      ;; if direct superclasses are not explicitly passed
      ;; we _must_ not change anything
      (call-next-method)))

;; KLUDGE this is not standard compliant: "Portable programs must not define methods on
;; shared-initialize." (for classes)
#+sbcl
(defmethod shared-initialize :around ((class computed-class) slot-names &rest args
                                      &key direct-slots &allow-other-keys)
  "Support :computed-in #f slot argument for documentation purposes."
  (remf-keywords args :direct-slots)
  (let* ((direct-slots (loop for direct-slot :in direct-slots
                             collect (progn
                                       (unless (getf direct-slot :computed-in)
                                         (remf-keywords direct-slot :computed-in))
                                       direct-slot))))
    (apply #'call-next-method class slot-names :direct-slots direct-slots args)))

