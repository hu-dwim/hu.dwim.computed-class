;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.computed-class)

(enable-sharp-boolean-syntax)

;;; These definitions need to be available by the time we are reading the other files, therefore
;;; they are in a standalone file.

(defmacro eval-always (&body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@body))

;; TODO: this should be in closer-mop?
(progn
  (defclass test ()
    (test-slot))

  (defparameter +unbound-slot-value+
    (let ((test (make-instance 'test)))
      (standard-instance-access test
                                (slot-definition-location
                                 (find 'test-slot (class-slots (find-class 'test))
                                       :key #'slot-definition-name))))
    "We use this symbol for unbound slot marking, so a quick eq is enough instead of a slot-boundp-using-class call.")

  (setf (find-class 'test) nil))

(defmacro debug-only (&body body)
  #+debug`(progn ,@body)
  #-debug(declare (ignore body)))

(defun inline-declaration ()
  (if *load-as-production?*
      '(inline
        incf-pulse current-pulse
        computed-state-value (setf computed-state-value)
        %computed-state-value (setf %computed-state-value) primitive-p
        invalidate-computed-state computed-state-or-nil find-slot
        computation-of-computed-state (setf computation-of-computed-state)
        standard-instance-access (setf standard-instance-access)
        copy-place-independent-slots-of-computed-state)
      (values)))

(defun file-header ()
  `(eval-always
    (declaim ,(inline-declaration))))

(defun setup-readtable ()
  (enable-sharp-boolean-syntax))

(defmacro standard-instance-access-form (object slot)
  `(standard-instance-access ,object
    ,(if (typep slot 'effective-slot-definition)
         (slot-definition-location slot)
         `(slot-definition-location ,slot))))

(defmacro setf-standard-instance-access-form (new-value object slot)
  ;; the default
  `(setf (standard-instance-access ,object
          ,(if (typep slot 'effective-slot-definition)
               (slot-definition-location slot)
               `(slot-definition-location ,slot)))
    ,new-value)
  
  ;; implementation specific overrides
  #+sbcl `(setf (sb-pcl::clos-slots-ref (sb-pcl::std-instance-slots ,object)
                 ,(if (typep slot 'effective-slot-definition)
                      (slot-definition-location slot)
                      `(slot-definition-location ,slot)))
           ,new-value))



