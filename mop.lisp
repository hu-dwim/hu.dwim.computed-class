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
  ((compute-as
     :type computed-state
     :accessor compute-as-of
     :initarg :compute-as)
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

;; this definition is here to allow a :computed argument for the slot definition
(defmethod initialize-instance :before ((computed-slot-definition computed-slot-definition) &key computed &allow-other-keys)
  (declare (ignore computed-slot-definition computed)))

(defclass computed-direct-slot-definition (computed-slot-definition standard-direct-slot-definition)
  ())

(defclass computed-direct-slot-definition-with-custom-accessors (computed-direct-slot-definition)
  ()
  (:documentation "This direct slot definition converts the :readers and :writers initargs to :computed-readers and :computed-writers effectively disabling the generation of default accessors."))

(defclass computed-effective-slot-definition (computed-slot-definition standard-effective-slot-definition)
  ())

(defmethod initialize-instance :around ((slot computed-direct-slot-definition-with-custom-accessors)
                                        &rest args &key readers writers &allow-other-keys)
  (remf-keywords args :readers :writers)
  (apply #'call-next-method slot :computed-readers readers :computed-writers writers args))

(defun compute-as-form-p (form)
  (and (consp form)
       (symbolp (first form))
       (get (first form) 'computed-as-macro-p)))

(defmethod direct-slot-definition-class ((class computed-class) &key initform (computed #f) &allow-other-keys)
  (if (or computed
          (compute-as-form-p initform))
      (find-class 'computed-direct-slot-definition)
      (call-next-method)))

(defmethod direct-slot-definition-class ((class computed-class*) &key initform (computed #f) &allow-other-keys)
  (if (or computed
          (compute-as-form-p initform))
      (find-class 'computed-direct-slot-definition-with-custom-accessors)
      (call-next-method)))

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
      ;; We collect and copy the readers and writers to the effective-slot, so we can access it
      ;; later when generating custom accessors.
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
          (let  ((method (ensure-method gf
                                        (ecase type
                                          (:reader
                                           `(lambda (object)
                                             (declare (optimize (speed 1))) ; (speed 1) to ignore compiler notes when defining accessors
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
                                             (declare (optimize (speed 1))) ; (speed 1) to ignore compiler notes when defining accessors
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
            thereis (subtypep class (find-class 'computed-object)))
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
                thereis (subtypep class (find-class 'computed-object)))
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
  "Support :computed #f slot argument for documentation purposes."
  (remf-keywords args :direct-slots)
  (let* ((direct-slots (loop for direct-slot :in direct-slots
                             collect (progn
                                       (unless (getf direct-slot :computed)
                                         (remf-keywords direct-slot :computed))
                                       direct-slot))))
    (apply #'call-next-method class slot-names :direct-slots direct-slots args)))
