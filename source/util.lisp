;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.computed-class)

(def (function io) unbound-slot-marker ()
  #*((:sbcl
      ;; without LOAD-TIME-VALUE the compiler dies
      (load-time-value sb-pcl::+slot-unbound+))
     (:ccl
      (ccl::%slot-unbound-marker))
     (t
      #.(not-yet-implemented/crucial-api 'unbound-slot-marker?))))

(def (function io) unbound-slot-marker? (value)
  (eq value (unbound-slot-marker)))

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
