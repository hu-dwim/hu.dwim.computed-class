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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:operate 'asdf:load-op :closer-mop))

(defpackage :computed-class-system
  (:use :cl :asdf :closer-mop)
  (:export
   #:optimize-declaration
   #:standard-instance-access-form
   #:setf-standard-instance-access-form))

(in-package :computed-class-system)

(defparameter *load-with-optimize-p* t)

(defun optimize-declaration ()
  (if *load-with-optimize-p*
      '(optimize (speed 3) (debug 0) (safety 0))
      (values)))

(defclass computed-class-file (cl-source-file)
  ())

(defun standard-instance-access-form (object slot-location)
  `(standard-instance-access ,object ,slot-location))

(defun setf-standard-instance-access-form (new-value object slot-location)
  `(setf (standard-instance-access ,object ,slot-location) ,new-value)
  #+sbcl `(setf (sb-pcl::clos-slots-ref (sb-pcl::std-instance-slots ,object) ,slot-location) ,new-value))

(defmethod perform :around ((op operation) (component computed-class-file))
  (let ((*features* *features*))
    (if *load-with-optimize-p*
        (pushnew :optimize *features*)
        (pushnew :debug *features*))
    #+sbcl(when *load-with-optimize-p*
            (pushnew :generate-custom-reader *features*)
            (pushnew :generate-custom-writer *features*))
    (call-next-method)))

(defsystem :computed-class
  :version "0.1"
  :author ("Attila Lendvai <attila.lendvai@gmail.com>"
	   "Tamás Borbély <tomi.borbely@gmail.com>"
	   "Levente Mészáros <levente.meszaros@gmail.com>")
  :maintainer ("Attila Lendvai <attila.lendvai@gmail.com>"
               "Tamás Borbély <tomi.borbely@gmail.com>"
	       "Levente Mészáros <levente.meszaros@gmail.com>")
  :licence "BSD"
  :description "Computed class is a class meta object which supports computed slots."
  :depends-on (:arnesi :closer-mop)
  :default-component-class computed-class-file
  :components
  ((:file "package")
   (:file "duplicates" :depends-on ("package"))
   (:file "computed-class" :depends-on ("duplicates"))
   (:file "clet" :depends-on ("computed-class"))
   (:file "swank-integration" :depends-on ("computed-class"))))

(defsystem :computed-class-test
  :description "Tests for the computed-class system."
  :depends-on (:computed-class :fiveam)
  :components
  ((:file "test")))

(defmethod perform :after ((op load-op) (system (eql (find-system :computed-class-test))))
  ;; globally enable the syntax in the repl thread
  (eval (read-from-string "(computed-class::enable-sharp-boolean-syntax)")))

(defmethod perform ((op test-op) (system (eql (find-system :computed-class))))
  (operate 'load-op :computed-class)
  (in-package :computed-class-test)
  (operate 'load-op :fiveam)
  (use-package :5am)
  (operate 'load-op :computed-class-test)
  (funcall (read-from-string "5am:run!"))
  (values))

(defmethod operation-done-p ((op test-op) (system (eql (find-system :computed-class))))
  nil)
