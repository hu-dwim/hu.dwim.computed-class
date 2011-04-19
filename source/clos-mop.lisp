;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.computed-class)

;;;;;;
;;; CLOS MOP related

(def method validate-superclass ((subclass computed-class) (superclass standard-class))
  (subtypep (class-of subclass) (class-of superclass)))

(def class computed-slot-definition (standard-slot-definition)
  ((computed-in
    :initform nil
    ;; TODO this brings up an error while compiling/loadin on ccl for some reason...
    #-ccl :type #-ccl (or (member t) computed-universe)
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

(def (class e) computed-direct-slot-definition (computed-slot-definition standard-direct-slot-definition)
  ())

(def class computed-direct-slot-definition-with-custom-accessors (computed-direct-slot-definition)
  ()
  (:documentation "This direct slot definition converts the :readers and :writers initargs to :computed-readers and :computed-writers effectively disabling the generation of default accessors."))

(def (class e) computed-effective-slot-definition (computed-slot-definition standard-effective-slot-definition)
  ())

(def class functional-slot-definition (standard-slot-definition)
  ((slot-value-function
    :type function
    :accessor slot-value-function-of
    :initarg :slot-value-function)
   (setf-slot-value-function
    :type function
    :accessor setf-slot-value-function-of
    :initarg :setf-slot-value-function))
  (:default-initargs :allocation :class))

(def class functional-direct-slot-definition (functional-slot-definition standard-direct-slot-definition)
  ())

(def class functional-effective-slot-definition (functional-slot-definition standard-effective-slot-definition)
  ())

(def method shared-initialize :around ((computed-slot-definition computed-direct-slot-definition) slot-names &rest args &key
                                       (initform nil initform-p) computed-in
                                       &allow-other-keys)
  ;; convert the initform into a compute-as* primitive form, assert and set computed-in properly
  (bind ((universe (when (and computed-in
                              (not (eq computed-in t)))
                     (find-computed-universe computed-in :otherwise `(:error "The specified :computed-in argument ~S is not a compute-as macro in any computed universe" ,computed-in))))
         ((:values compute-as-form? nil initform-universe) (compute-as-form? initform))
         (primitive-compute-as-form (if compute-as-form?
                                        (expand-to-primitive-compute-as-form initform)
                                        initform)))
    (when (and initform-p
               primitive-compute-as-form)
      (if universe
          (assert (or (atom primitive-compute-as-form)
                      (eq universe initform-universe)) ()
                      ":computed-in and the :initform parameters are not consistent: ~S is not computed in ~S" initform universe)
          (setf universe initform-universe)))
    (apply #'call-next-method computed-slot-definition slot-names
           (append
            (list :computed-in (or universe
                                   computed-in))
            (when initform-p
              (list :initform primitive-compute-as-form
                    :initfunction (if primitive-compute-as-form
                                      (compile nil `(lambda () ,primitive-compute-as-form))
                                      (constantly nil))))
            args))))

(def method initialize-instance :around ((slot computed-direct-slot-definition-with-custom-accessors) &rest args &key readers writers &allow-other-keys)
  (remove-from-plistf args :readers :writers)
  (apply #'call-next-method slot :computed-readers readers :computed-writers writers args))

(def function needs-to-be-computed-direct-slot-p (slot-initargs)
  (let ((initform (getf slot-initargs :initform)))
    (or (getf slot-initargs :computed-in)
        (compute-as-form? initform))))

(def method direct-slot-definition-class ((class computed-class) &rest slot-initargs
                                         &key slot-value-function setf-slot-value-function &allow-other-keys)
  (cond ((or slot-value-function setf-slot-value-function)
         (find-class 'functional-direct-slot-definition))
        ((needs-to-be-computed-direct-slot-p slot-initargs)
         (find-class 'computed-direct-slot-definition))
        (t
         (call-next-method))))

(def method effective-slot-definition-class ((class computed-class) &key &allow-other-keys)
  (declare (special %effective-slot-definition-class%))
  (aif %effective-slot-definition-class%
       (find-class it)
       (call-next-method)))

(def function needs-to-be-computed-effective-slot-p (direct-slot-definitions)
  (find-if (lambda (direct-slot-definition)
             (typep direct-slot-definition 'computed-direct-slot-definition))
           direct-slot-definitions))

(def function needs-to-be-functional-effective-slot-p (direct-slot-definitions)
  (find-if (lambda (direct-slot-definition)
             (typep direct-slot-definition 'functional-direct-slot-definition))
           direct-slot-definitions))

(def method compute-effective-slot-definition ((class computed-class) name direct-slot-definitions)
  (declare (type list direct-slot-definitions))
  ;; TODO: it is unclear what to do when the direct slot definitions have different computed-in specifications
  (let ((%effective-slot-definition-class%
         (cond ((needs-to-be-computed-effective-slot-p direct-slot-definitions)
                'computed-effective-slot-definition)
               ((needs-to-be-functional-effective-slot-p direct-slot-definitions)
                'functional-effective-slot-definition))))
    (declare (special %effective-slot-definition-class%))
    (aprog1
        (call-next-method)
      ;; We collect and copy the readers and writers to the effective-slot, so we can access it
      ;; later when generating custom accessors.
      (cond ((typep it 'computed-effective-slot-definition)
             (setf (computed-in-of it) (some (lambda (slot)
                                               (when (typep slot 'computed-direct-slot-definition)
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
               (map nil 'ensure-generic-function-for-accessor (computed-writers-of it))))
            ((typep it 'functional-effective-slot-definition)
             (setf (slot-value-function-of it)
                   (compile nil (some 'slot-value-function-of direct-slot-definitions)))
             (setf (setf-slot-value-function-of it)
                   (compile nil (some 'setf-slot-value-function-of direct-slot-definitions))))))))

(def macro slot-value-using-class-body (object slot)
  (declare (type (or symbol effective-slot-definition) slot))
  `(let ((slot-value (standard-instance-access-form ,object ,slot)))
    (when (unbound-slot-marker? slot-value)
      (error 'unbound-slot
             :name ,(if (symbolp slot)
                        `(slot-definition-name ,slot)
                        `(quote ,(slot-definition-name slot)))
             :instance ,object))
    (if (computed-state-p slot-value)
        (%computed-state-value slot-value)
        slot-value)))

(def macro setf-slot-value-using-class-body (new-value object slot)
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
                (debug-only
                  (setf (cs-place-descriptor ,new-value) ,slot))
                (setf-standard-instance-access-form slot-value ,object ,slot)))
          (invalidate-computed-state slot-value)
          slot-value)
        (if (computed-state-p slot-value)
            ;; it also clears the dependency list, making it basically an uncomputed slot. but this slot will still be recorded
            ;; in dependencies and invalidate its dependencies when it gets updated. (c-in in Cells)
            ;; TODO: this will neither clear nor set the cs-compute-as to a constantly lambda, which is probably a bad thing.
            ;; TODO: e.g. currently :always computed-state's silently ignore setf-ing a constant in them.
            (setf (%computed-state-value slot-value) ,new-value)
            ;; we just write the non computed-state new value in the slot that currently holds a non computed-state value or is unbound
            ;; TODO we could automatically wrap the value in a computed state but it's not trivial and we don't need it right now
            (setf-standard-instance-access-form ,new-value ,object ,slot)))))

(def method slot-value-using-class ((class computed-class) (object computed-object) (slot computed-effective-slot-definition))
  (declare #.(optimize-declaration))
  (slot-value-using-class-body object slot))

(def method slot-value-using-class ((class computed-class) (object computed-object) (slot functional-effective-slot-definition))
  (declare #.(optimize-declaration))
  (funcall (the function (slot-value-function-of slot)) class object slot #'call-next-method))

(def method (setf slot-value-using-class) (new-value (class computed-class) (object computed-object) (slot computed-effective-slot-definition))
  (declare #.(optimize-declaration))
  (setf-slot-value-using-class-body new-value object slot))

(def method (setf slot-value-using-class) (new-value (class computed-class) (object computed-object) (slot functional-effective-slot-definition))
  (declare #.(optimize-declaration))
  (funcall (the function (setf-slot-value-function-of slot)) new-value class object slot #'call-next-method))

(def method slot-boundp-using-class ((class computed-class) (object computed-object) (slot computed-effective-slot-definition))
  (declare #.(optimize-declaration))
  (not (unbound-slot-marker? (standard-instance-access-form object slot))))

(def method slot-boundp-using-class ((class computed-class) (object computed-object) (slot functional-effective-slot-definition))
  (declare #.(optimize-declaration))
  #t)

(def method slot-makunbound-using-class ((class computed-class) (object computed-object) (slot computed-effective-slot-definition))
  (declare #.(optimize-declaration))
  (setf-standard-instance-access-form (unbound-slot-marker) object slot))

(def method slot-makunbound-using-class ((class computed-class) (object computed-object) (slot functional-effective-slot-definition))
  (declare #.(optimize-declaration))
  (error "The functional slot ~A in class ~A cannot be unbound." (slot-definition-name slot) (class-name class)))

(def function ensure-generic-function-for-accessor (accessor-name type)
  (ensure-generic-function accessor-name :lambda-list (ecase type
                                                        (:reader '(object))
                                                        (:writer '(new-value object)))))

;;; make sure computed-object is among the supers (thanks to Pascal Constanza)
(def method initialize-instance :around ((class computed-class) &rest initargs &key direct-superclasses)
  (declare (dynamic-extent initargs))
  (if (loop for class in direct-superclasses
            thereis (ignore-errors (subtypep class (find-class 'computed-object))))
      (call-next-method)
      (apply #'call-next-method
             class
             :direct-superclasses (append direct-superclasses (list (find-class 'computed-object)))
             initargs)))

(def method reinitialize-instance :around ((class computed-class) &rest initargs &key (direct-superclasses '() direct-superclasses-p))
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
(def method shared-initialize :around ((class computed-class) slot-names &rest args &key direct-slots &allow-other-keys)
  "Support :computed-in #f slot argument for documentation purposes."
  (remove-from-plistf args :direct-slots)
  (let* ((direct-slots (loop for direct-slot :in direct-slots
                             collect (progn
                                       (unless (getf direct-slot :computed-in)
                                         (remove-from-plistf direct-slot :computed-in))
                                       direct-slot))))
    (apply #'call-next-method class slot-names :direct-slots direct-slots args)))
