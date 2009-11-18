;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.computed-class)

(progn
  ;; TODO: this should be in closer-mop?
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

;; TODO '(inline standard-instance-access (setf standard-instance-access))

(def macro standard-instance-access-form (object slot)
  `(standard-instance-access ,object
    ,(if (typep slot 'effective-slot-definition)
         (slot-definition-location slot)
         `(slot-definition-location ,slot))))

(def macro setf-standard-instance-access-form (new-value object slot)
  #*((:sbcl `(setf (sb-pcl::clos-slots-ref (sb-pcl::std-instance-slots ,object)
                                           ,(if (typep slot 'effective-slot-definition)
                                                (slot-definition-location slot)
                                                `(slot-definition-location ,slot)))
                   ,new-value))
     (t `(setf (standard-instance-access ,object
                                         ,(if (typep slot 'effective-slot-definition)
                                              (slot-definition-location slot)
                                              `(slot-definition-location ,slot)))
               ,new-value))))
