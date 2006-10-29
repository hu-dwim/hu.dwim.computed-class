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

(in-package :computed-class-test)

(eval-always
  (import (let ((*package* (find-package :computed-class)))
            (read-from-string "(find-slot computed-state-or-nil computed-effective-slot-definition current-pulse)"))))

(computed-class::enable-sharp-boolean-syntax)

(def-suite :computed-class :description "Computed class tests")

(in-suite :computed-class)

(define-computed-universe compute-as :name "Global computed-class-test universe")
;; TODO make tests for (define-computed-universe separated-compute-as :name "Separated computed-class-test universe")

;;;;;;;;;;;;;;;;;;
;;; defclass tests

(test computed-class/defclass/1
  (defclass computed-1 ()
    ()
    (:metaclass computed-class)))

(test computed-class/defclass/2
  (defclass computed-2 ()
    ((slot-a
      :initform (compute-as 1)))
    (:metaclass computed-class)))

(test computed-class/defclass/3
  (let ((class
         (defclass computed-3 ()
           ((a)
            (b :computed #f)
            (c :computed #t))
           (:metaclass computed-class))))
    (flet ((computed-slot-p (slot-name)
             (typep (find-slot class slot-name) 'computed-effective-slot-definition)))
      (finalize-inheritance class)
      (is (not (computed-slot-p 'a)))
      (is (not (computed-slot-p 'b)))
      (is (computed-slot-p 'c)))))

(test computed-class/defclass/4
  (defclass computed-4 ()
    ((slot-a
      :initform (compute-as 0))
     (slot-b
      :initform (compute-as (1+ (slot-a-of self)))))
    (:metaclass computed-class)))

;;;;;;;;;;;;;;;;;;
;;; Instance tests

(defclass computed-test ()
  ((slot-a
    :accessor slot-a-of
    :initarg :slot-a
    :initform (compute-as 0))
   (slot-b
    :accessor slot-b-of
    :initarg :slot-b
    :initform (compute-as (1+ (slot-a-of self)))))
  (:metaclass computed-class))

(test computed-class/compute/1
  (let ((object (make-instance 'computed-test
                               :slot-a (compute-as 1)
                               :slot-b (compute-as (1+ (slot-a-of self))))))
    (is (= 1 (slot-a-of object)))
    (is (= 2 (slot-b-of object)))
    (setf (slot-a-of object) 2)
    (is (= 2 (slot-a-of object)))
    (is (= 3 (slot-b-of object)))))

(test computed-class/compute/2
  (let* ((object-1 (make-instance 'computed-test
                                  :slot-a (compute-as 1)
                                  :slot-b (compute-as (1+ (slot-a-of self)))))
         (object-2 (make-instance 'computed-test
                                  :slot-a (compute-as (+ (slot-a-of object-1) (slot-b-of object-1)))
                                  :slot-b (compute-as (1+ (slot-a-of self))))))
    (is (= 4 (slot-b-of object-2)))
    (setf (slot-a-of object-1) 2)
    (is (= 6 (slot-b-of object-2)))))

;; TODO: send a bug report to SBCL's list
(test computed-class/compute/3
  (setf (find-class 'sbcl-class-cache-computed-test) nil)
  (defclass sbcl-class-cache-computed-test ()
    ((slot-a :accessor slot-a-of :initarg :slot-a)
     (slot-b :accessor slot-b-of :initarg :slot-b))
    (:metaclass computed-class))
  (let ((object (make-instance 'sbcl-class-cache-computed-test :slot-a (compute-as 1) :slot-b 1)))
    (slot-a-of object)
    (slot-b-of object))
  (defclass sbcl-class-cache-computed-test ()
    ((slot-a :accessor slot-a-of :initarg :slot-a :computed #t)
     (slot-b :accessor slot-b-of :initarg :slot-b :computed #t))
    (:metaclass computed-class))
  (let ((object (make-instance 'sbcl-class-cache-computed-test
                               :slot-a (compute-as 1)
                               :slot-b (compute-as (1+ (slot-a-of self))))))
    (is (= 1 (slot-a-of object)))
    ;; the next call does not call slot-value-using-class probably because of some accessor method cache?
    (is (= 2 (slot-b-of object)))
    (setf (slot-a-of object) 2)
    ;; the next call does not call slot-value-using-class probably because of some accessor method cache?
    ;; even slot-value does not call svuc?!
    (is (= 3 (slot-b-of object)))))

;;;;;;;;;;;;;;;;;;
;;; clet tests

(test computed-class/clet/1
  (clet ((a (compute-as 1))
         (b (compute-as (1+ a)))
         (c (compute-as (+ a b))))
    (is (= 3 c))
    (setf a 2)
    (is (= 5 c))))

(test computed-class/pulse/1
  (let* ((object (make-instance 'computed-test
                                :slot-a (compute-as 1)
                                :slot-b (compute-as (1+ (slot-a-of self))))))
    (flet ((current-pulse ()
             (current-pulse (computed-state-or-nil (class-of object) object (find-slot (class-of object) 'slot-a)))))
      (let ((pulse (current-pulse)))
        (setf (slot-a-of object) 2)
        (is (= (current-pulse) (+ 1 pulse)))
        (slot-b-of object)
        (is (= (current-pulse) (+ 1 pulse)))))))

(test computed-class/circularity/1
  (let* ((circularity #f)
         (flag #f)
         (object (make-instance 'computed-test
                                :slot-a (compute-as
                                          (when (or circularity flag)
                                            (slot-b-of self)))
                                :slot-b (compute-as
                                          (when (or circularity (not flag))
                                            (slot-a-of self))))))
    (setf flag #f)
    (invalidate-computed-slot object 'slot-a)
    (invalidate-computed-slot object 'slot-b)
    (is (null (slot-a-of object)))
    (is (null (slot-b-of object)))

    (setf flag #t)
    (invalidate-computed-slot object 'slot-a)
    (invalidate-computed-slot object 'slot-b)
    (is (null (slot-a-of object)))
    (is (null (slot-b-of object)))
    
    (setf circularity #t)
    (invalidate-computed-slot object 'slot-a)
    (invalidate-computed-slot object 'slot-b)
    (signals error (slot-a-of object))
    (signals error (slot-b-of object))))

;;;;;;;;;;;;;;;;
;;; Timing tests

(defclass standard-test ()
  ((slot-a
    :accessor slot-a-of
    :initarg :slot-a
    :initform 0)
   (slot-b
    :accessor slot-b-of
    :initarg :slot-b
    :initform 0)))

(test computed-class/timing/1
  (finishes
    (flet ((measure (object)
             (setf (slot-a-of object) 0)
             #+sbcl(sb-ext:gc :full t)
             (time
              (dotimes (counter 2000000)
                (slot-b-of object)))))
      (measure (make-instance 'standard-test))
      (measure (make-instance 'computed-test
                              :slot-a (compute-as 0)
                              :slot-b (compute-as (1+ (slot-a-of self))))))))

(test computed-class/timing/2
  (finishes
    (flet ((measure (object)
             #+sbcl(sb-ext:gc :full t)
             (time
              (dotimes (counter 1000000)
                (setf (slot-a-of object) counter)
                (slot-b-of object)))))
      (measure (make-instance 'standard-test))
      (measure (make-instance 'computed-test
                              :slot-a (compute-as 0)
                              :slot-b (compute-as (1+ (slot-a-of self))))))))

#|
;; TODO: the following code could be generated by computed-class to make readers really fast
;; TODO: approximately 4-5 times slower than standard readers
;; TODO: without this readers are 10-15 times slower than standard readers
;; TODO: when readers need to recalculate using a single slot they become 20-30 times slower than standard readers
;; TODO: capture these in the lambda, because they are known during class definition
(defvar *slot* (find-slot (find-class 'computed-test) 'slot-b))
(defvar *slot-location* (slot-definition-location (find-slot (find-class 'computed-test) 'slot-b)))
(defvar *class* (find-class 'computed-test))

(defmethod slot-b-of ((object computed-test))
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (inline %slot-value-using-class))
  ;; this is the body of slot-value-using-class
  (%slot-value-using-class *class* object *slot*)
  (standard-instance-access object *slot-location*))
|#
