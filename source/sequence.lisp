;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.computed-class)

;;;;;;
;;; Cons

(def (class* e) computed-cons ()
  ((car :type t :computed-in t)
   (cdr :type t :computed-in t))
  (:metaclass computed-class))

(def (function e) make-computed-cons (car cdr)
  (make-instance 'computed-cons :car car :cdr cdr))

(def (function e) make-computed-list (&rest elements)
  (when elements
    (make-instance 'computed-cons :car (car elements) :cdr (make-computed-list (cdr elements)))))

;;;;;;
;;; Sequence

(def (class* e) computed-sequence (sequence)
  ((elements :type sequence))
  (:metaclass computed-class))

(def (function e) make-computed-sequence (&rest elements)
  (make-instance 'computed-sequence :elements elements))

(def method sb-sequence:emptyp ((sequence computed-sequence))
  (emptyp (elements-of sequence)))

(def method sb-sequence:length ((sequence computed-sequence))
  (length (elements-of sequence)))

(def method sb-sequence:elt ((sequence computed-sequence) index)
  (bind ((element (elt (elements-of sequence) index)))
    (if (computed-state-p element)
        (computed-state-value element)
        element)))

(def method (setf sb-sequence:elt) (new-value (sequence computed-sequence) index)
  (bind ((element (elt (elements-of sequence) index)))
    (if (computed-state-p element)
        (setf (computed-state-value element) new-value)
        (setf (elt (elements-of sequence) index) new-value))))

(def method sb-sequence:adjust-sequence ((sequence computed-sequence) length &key (initial-element nil initial-element?) initial-contents)
  (setf (elements-of sequence) (if initial-element?
                                   (make-array (list length) :initial-element initial-element)
                                   (make-array (list length) :initial-contents (append initial-contents (make-list (- length (length initial-contents))))))))

(def method sb-sequence:subseq ((sequence computed-sequence) start &optional end)
  (make-instance (class-of sequence) :elements (subseq (elements-of sequence) start end)))

(def method sb-sequence:replace ((sequence-1 computed-sequence) (sequence-2 computed-sequence) &rest args &key start1 end1 start2 end2)
  (declare (ignore start1 end1 start2 end2))
  (apply #'replace (elements-of sequence-1) (elements-of sequence-2) args)
  sequence-1)

(def method sb-sequence:make-sequence-like ((sequence computed-sequence) length &key (initial-element nil initial-element?) initial-contents)
  (make-instance (class-of sequence) :elements (if initial-element?
                                                   (make-array (list length) :initial-element initial-element)
                                                   (make-array (list length) :initial-contents (append initial-contents (make-list (- length (length initial-contents))))))))
