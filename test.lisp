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
            (read-from-string "(find-slot computed-state-or-nil computed-effective-slot-definition current-pulse
                                slot-value-using-class-body setf-slot-value-using-class-body
                                enable-sharp-boolean-syntax standard-instance-access-form computed-state-p
                                log.dribble log.debug log.info log.warn log.error
                                cs-attached-p)"))))

(enable-sharp-boolean-syntax)

(def-suite :computed-class :description "Computed class tests")

(in-suite :computed-class)

(define-computed-universe compute-as :name "Default computed-class-test universe")
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
            #+sbcl(b :computed-in nil)
            (c :computed-in compute-as))
           (:metaclass computed-class))))
    (flet ((computed-slot-p (slot-name)
             (typep (find-slot class slot-name) 'computed-effective-slot-definition)))
      (finalize-inheritance class)
      (is (not (computed-slot-p 'a)))
      #+sbcl(is (not (computed-slot-p 'b)))
      (is (computed-slot-p 'c)))))

(test computed-class/defclass/4
  (defclass computed-4 ()
    ((slot-a
      :initform (compute-as 0))
     (slot-b
      :initform (compute-as 1)))
    (:metaclass computed-class)))

(test computed-class/defclass-with-accessors/1
  (let ((class
         (defclass computed-5 ()
           ((a)
            (c :computed-in compute-as))
           (:metaclass computed-class*))))
    (flet ((computed-slot-p (slot-name)
             (typep (find-slot class slot-name) 'computed-effective-slot-definition)))
      (finalize-inheritance class)
      (is (not (computed-slot-p 'a)))
      (is (computed-slot-p 'c)))))

(test computed-class/subclassing/1
  (defclass super ()
    ((a
      :accessor a-of
      :initform (compute-as 42))
     (b
      :accessor b-of
      :initform (compute-as (1+ (a-of -self-)))))
    (:metaclass computed-class*))

  (defclass level0-1 (super)
    ((x))
    (:metaclass computed-class*))
  
  (defclass level0-2 (super)
    ((y))
    (:metaclass computed-class*))

  (defclass level1-1 (level0-1)
    ((z)))

  (defclass sub (level1-1 level0-2)
    ((b
      :accessor b-of
      :initform (compute-as 0))
     (a
      :accessor a-of
      :initform (compute-as 1)))
    (:metaclass computed-class*))
  
  (let ((sub (make-instance 'sub)))
    (is (= (a-of sub) 1))
    (is (= (b-of sub) 0))))

;;;;;;;;;;;;;;;;;;
;;; Instance tests

(defclass computed-test ()
  ((slot-a
    :accessor slot-a-of
    :initarg :slot-a
    :computed-in compute-as)
   (slot-b
    :accessor slot-b-of
    :initarg :slot-b
    :computed-in compute-as))
  (:metaclass computed-class*))

(test computed-class/boundp/1
  (let ((object (make-instance 'computed-test)))
    (signals unbound-slot (slot-a-of object))
    (setf (slot-a-of object) (compute-as 1))
    (setf (slot-b-of object) (compute-as (1+ (slot-a-of -self-))))
    (is (= 2 (slot-b-of object)))
    (setf (slot-a-of object) 2)
    (is (= 2 (slot-a-of object)))
    (is (= 3 (slot-b-of object)))
    (slot-makunbound object 'slot-a)
    (is (not (slot-boundp object 'slot-a)))
    (signals unbound-slot (slot-a-of object))))

(test computed-class/compute/1
  (let ((object (make-instance 'computed-test
                               :slot-a (compute-as 1)
                               :slot-b (compute-as (1+ (slot-a-of -self-))))))
    (is (= 1 (slot-a-of object)))
    (is (= 2 (slot-b-of object)))
    (setf (slot-a-of object) 2)
    (is (= 2 (slot-a-of object)))
    (is (= 3 (slot-b-of object)))))

(test computed-class/compute/2
  (let* ((object-1 (make-instance 'computed-test
                                  :slot-a (compute-as 1)
                                  :slot-b (compute-as (1+ (slot-a-of -self-)))))
         (object-2 (make-instance 'computed-test
                                  :slot-a (compute-as (+ (slot-a-of object-1) (slot-b-of object-1)))
                                  :slot-b (compute-as (1+ (slot-a-of -self-))))))
    (is (= 4 (slot-b-of object-2)))
    (setf (slot-a-of object-1) 2)
    (is (= 6 (slot-b-of object-2)))))

(test computed-class/compute/3
  (let* ((object-1 (make-instance 'computed-test
                                  :slot-b (compute-as (1+ (slot-a-of -self-)))))
         (object-2 (make-instance 'computed-test
                                  :slot-a (compute-as (+ (slot-a-of object-1) (slot-b-of object-1)))
                                  :slot-b (compute-as (1+ (slot-a-of -self-))))))
    (signals unbound-slot (slot-a-of object-1))
    (setf (slot-a-of object-1) 0)
    (is (= 0 (slot-a-of object-1)))
    (is (= 2 (slot-b-of object-2)))
    (setf (slot-a-of object-1) 2)
    (setf (slot-b-of object-2) (compute-as (* 2 (slot-a-of -self-))))
    (is (= 5 (slot-a-of object-2)))
    (is (= 10 (slot-b-of object-2)))))

;; TODO: send a bug report to SBCL's list
(test computed-class/compute/4
  (setf (find-class 'sbcl-class-cache-computed-test) nil)
  (defclass sbcl-class-cache-computed-test ()
    ((slot-a :accessor slot-a-of :initarg :slot-a)
     (slot-b :accessor slot-b-of :initarg :slot-b))
    (:metaclass computed-class*))
  (let ((object (make-instance 'sbcl-class-cache-computed-test :slot-a (compute-as 1) :slot-b 1)))
    (slot-a-of object)
    (slot-b-of object))
  (defclass sbcl-class-cache-computed-test ()
    ((slot-a :accessor slot-a-of :initarg :slot-a :computed-in compute-as)
     (slot-b :accessor slot-b-of :initarg :slot-b :computed-in compute-as))
    (:metaclass computed-class*))
  (let ((object (make-instance 'sbcl-class-cache-computed-test
                               :slot-a (compute-as 1)
                               :slot-b (compute-as (1+ (slot-a-of -self-))))))
    (is (= 1 (slot-a-of object)))
    ;; the next call does not call slot-value-using-class probably because of some accessor method cache?
    (is (= 2 (slot-b-of object)))
    (setf (slot-a-of object) 2)
    ;; the next call does not call slot-value-using-class probably because of some accessor method cache?
    ;; even slot-value does not call svuc?!
    (is (= 3 (slot-b-of object)))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Reconfiguration tests

(test computed-class/reconfigure/1
  (let* ((object (make-instance 'computed-test)))
    (setf (slot-a-of object) nil)
    (setf (slot-b-of object) nil)
    (setf (slot-a-of object) (compute-as 1))
    (setf (slot-b-of object) (compute-as (1+ (slot-a-of -self-))))
    (is (= 2 (slot-b-of object)))
    (setf (slot-a-of object) (compute-as (* 6 7)))
    (is (= 43 (slot-b-of object)))
    (setf (slot-a-of object) nil)
    (is (null (slot-a-of object)))
    (setf (slot-b-of object) (compute-as (not (slot-a-of -self-))))
    (is (not (null (slot-b-of object))))))

(test computed-class/reconfigure/2
  (let ((object (make-instance 'computed-test)))
    (flet ((current-pulse ()
             (awhen (computed-state-or-nil object (find-slot (class-of object) 'slot-b))
               (current-pulse it))))
      (setf (slot-a-of object) 1)
      (setf (slot-b-of object) (compute-as (1+ (slot-a-of object))))
      (is (= 1 (slot-a-of object)))
      (is (= 2 (slot-b-of object)))
      (make-slot-uncomputed object 'slot-a)
      (let ((pulse (current-pulse)))
        (is (= 1 (slot-a-of object)))
        (is (= 2 (slot-b-of object)))
        (setf (slot-a-of object) 42)
        (is (not (computed-state-p (standard-instance-access-form object (find-slot (class-of object) 'slot-a)))))
        (is (= 2 (slot-b-of object)))
        (is (= pulse (current-pulse)))
        (setf (slot-a-of object) (compute-as 42)) ; make it computed again
        (is (= (+ pulse 1) (current-pulse)))
        (is (= 2 (slot-b-of object))) ; dependency should not have been registered, so it should not get recalculated
        (invalidate-computed-slot object 'slot-b)
        (is (= 43 (slot-b-of object)))))))

;;;;;;;;;;;;;;;;;;
;;; clet tests

(test computed-class/clet/1
  (clet ((a (compute-as 1))
         (b (compute-as (1+ a)))
         (c (compute-as (+ a b))))
    (is (= c 3))
    (setf a 2)
    (is (= c 5))
    (signals error (setf a (compute-as 42))) ; this is not the way to do it
    (let ((old-a-state a-state))
      (is (cs-attached-p old-a-state))
      (setf a-state (compute-as 42)) ; this setf also invalidates the state of 'b' and 'c'
      (is (not (cs-attached-p old-a-state))))
    (is (= c 85))
    (setf a 43)
    (is (= c 87))))

(test computed-class/clet/2
  (clet ((a 42)
         (b (compute-as (1+ a)))
         (c (compute-as (1+ b))))
    (is (= a 42))
    (is (= b 43))
    (is (= c 44))
    (setf a 2)
    (is (= a 2))                        ; does not invalidate anything, that's a simple let* binding
    (is (= b 43))
    (is (= c 44))
    (invalidate-computed-state b-state)
    (is (= b 3))
    (is (= c 4))))

(test computed-class/clet/3
  ;; same as computed-class/clet/2, but with variable capturing
  (let (a-reader
        a-writer
        b-reader
        b-writer
        b-state-reader
        c-reader)
    (clet ((a 42)
           (b (compute-as (1+ a)))
           (c (compute-as (1+ b))))
      (setf a-reader (lambda () a)
            b-reader (lambda () b)
            c-reader (lambda () c)
            a-writer (lambda (value) (setf a value))
            b-writer (lambda (value) (setf b value))
            b-state-reader (lambda () b-state)))
    (is (= (funcall a-reader) 42))
    (is (= (funcall b-reader) 43))
    (is (= (funcall c-reader) 44))
    (funcall a-writer 2)
    (is (= (funcall a-reader) 2))                      ; does not invalidate anything, that's a simple let* binding
    (is (= (funcall b-reader) 43))
    (is (= (funcall c-reader) 44))
    (invalidate-computed-state (funcall b-state-reader)) ; should also invalidate 'c'
    (is (= (funcall b-reader) 3))
    (is (= (funcall c-reader) 4))))

(test computed-class/clet/4
  (clet ((a (compute-as 2))
         (object (make-instance 'computed-test
                                :slot-a (compute-as (1+ a))
                                :slot-b (compute-as (1+ (slot-a-of -self-)))))
         (b (compute-as (+ a (slot-b-of object)))))
    (is (= a 2))
    (is (= b 6))
    (is (= (slot-a-of object) 3))
    (is (= (slot-b-of object) 4))
    (setf (slot-a-of object) 42)
    (is (= a 2))
    (is (= b 45))
    (is (= (slot-a-of object) 42))
    (is (= (slot-b-of object) 43))))

(test computed-class/pulse/1
  (let* ((object (make-instance 'computed-test
                                :slot-a (compute-as 1)
                                :slot-b (compute-as (1+ (slot-a-of -self-))))))
    (flet ((current-pulse ()
             (awhen (computed-state-or-nil object (find-slot (class-of object) 'slot-a))
               (current-pulse it))))
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
                                            (slot-b-of -self-)))
                                :slot-b (compute-as
                                          (when (or circularity (not flag))
                                            (slot-a-of -self-))))))
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
              (dotimes (counter 4000000)
                (slot-b-of object)))))
      (measure (make-instance 'standard-test))
      (measure (make-instance 'computed-test
                              :slot-a (compute-as 0)
                              :slot-b (compute-as (1+ (slot-a-of -self-))))))))

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
                              :slot-b (compute-as (1+ (slot-a-of -self-))))))))
