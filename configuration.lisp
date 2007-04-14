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

(enable-sharp-boolean-syntax)

;;; These definitions need to be available by the time we are reading the other files, therefore
;;; they are in a standalone file.

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
  (if *load-with-debug-p*
      (values)
      '(inline
        incf-pulse current-pulse
        computed-state-value (setf computed-state-value)
        %computed-state-value (setf %computed-state-value) primitive-p
        invalidate-computed-state computed-state-or-nil find-slot
        computation-of-computed-state (setf computation-of-computed-state)
        standard-instance-access (setf standard-instance-access)
        copy-place-independent-slots-of-computed-state)))

(defun file-header ()
  `(eval-always
    (declaim ,(inline-declaration))
    (setup-readtable)))

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



