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

(in-package :cl-user)

(defpackage :computed-class
  (:nicknames :cc)

  (:shadow #:log)
  
  (:use :cl :closer-mop :arnesi :computed-class-system)

  (:export #:computed-class
           #:computed-class*
           #:define-computed-universe
           #:-self-
           #:-current-value-
           #:make-slot-uncomputed
           #:clet
           #:defcfun
           #:computed-state-value
           #:computation-of-computed-state)

  ;; for debug purposes
  (:export #:computed-state-for
           #:invalidate-computed-slot
           #:invalidate-computed-state
           #:recompute-slot
           #:recompute-computed-state))

(defpackage :computed-class-test
  (:use :cl :computed-class :closer-mop :arnesi))

(in-package :computed-class)

(deflogger log ()
  :level +warn+
  :compile-time-level #+optimize +warn+ #-optimize +dribble+
  :appender (make-instance 'brief-stream-log-appender :stream *debug-io*))
