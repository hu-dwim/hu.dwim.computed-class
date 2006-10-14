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

(def-suite :computed-class :description "Computed class tests")

(in-suite :computed-class)

(test computed-class/defclass/1
  (defclass computed-test ()
    ()
    (:metaclass computed-class)))

(test computed-class/defclass/2
  (defclass computed-test ()
    ((slot-a
      :initform (compute-as 1)))
    (:metaclass computed-class)))

(test computed-class/defclass/3
  (let ((class
         (defclass computed-test ()
           ((a)
            (b :computed #t))
           (:metaclass computed-class))))
    (flet ((computed-slot-p (slot-name)
             (typep (find-slot class slot-name) 'computed-effective-slot-definition)))
      (finalize-inheritance class)
      (is (not (computed-slot-p 'a)))
      (is (computed-slot-p 'b)))))

(test computed-class/defclass/test
  (defclass computed-test ()
    ((slot-a
      :accessor slot-a-of
      :initarg :slot-a
      :initform (compute-as 0))
     (slot-b
      :accessor slot-b-of
      :initarg :slot-b
      :initform (compute-as (1+ (slot-a-of self)))))
    (:metaclass computed-class)))

(test computed-class/compute/1
  (run 'computed-class/defclass/test)
  (let ((object (make-instance 'computed-test
                               :slot-a (compute-as 1)
                               :slot-b (compute-as (1+ (slot-a-of self))))))
    (is (= 1 (slot-a-of object)))
    (is (= 2 (slot-b-of object)))
    (setf (slot-a-of object) 2)
    (is (= 2 (slot-a-of object)))
    (is (= 3 (slot-b-of object)))))

(test computed-class/compute/2
  (run 'computed-class/defclass/test)
  (let* ((object-1 (make-instance 'computed-test
                                  :slot-a (compute-as 1)
                                  :slot-b (compute-as (1+ (slot-a-of self)))))
         (object-2 (make-instance 'computed-test
                                  :slot-a (compute-as (+ (slot-a-of object-1) (slot-b-of object-1)))
                                  :slot-b (compute-as (1+ (slot-a-of self))))))
    (is (= 4 (slot-b-of object-2)))
    (setf (slot-a-of object-1) 2)
    (is (= 6 (slot-b-of object-2)))))

(test computed-class/pulse/1
  (run 'computed-class/defclass/test)
  (let* ((object (make-instance 'computed-test
                                :slot-a (compute-as 1)
                                :slot-b (compute-as (1+ (slot-a-of self)))))
         (pulse *pulse*))
    (setf (slot-a-of object) 2)
    (is (= *pulse* (+ 1 pulse)))
    (slot-b-of object)
    (is (= *pulse* (+ 1 pulse)))))

(test computed-class/circularity/1
  (run 'computed-class/defclass/test)
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

(test computed-class/timing/1
  (run 'computed-class/defclass/test)
  (defclass standard-test ()
    ((slot-a
      :accessor slot-a-of
      :initarg :slot-a
      :initform 0)
     (slot-b
      :accessor slot-b-of
      :initarg :slot-b
      :initform 0)))
  (finishes
    (flet ((measure (object)
             (time
              (dotimes (counter 100000)
                (setf (slot-a-of object) counter)
                (slot-b-of object)))))
      (measure (make-instance 'standard-test))
      (measure (make-instance 'computed-test
                              :slot-a (compute-as 0)
                              :slot-b (compute-as (1+ (slot-a-of self))))))))
