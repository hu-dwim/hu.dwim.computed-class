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
  (flet ((try (system)
           (unless (asdf:find-system system nil)
             (warn "Trying to install required dependency: ~S" system)
             (when (find-package :asdf-install)
               (funcall (read-from-string "asdf-install:install") system))
             (unless (asdf:find-system system nil)
               (error "The ~A system requires ~A." (or *compile-file-pathname* *load-pathname*) system)))
           (asdf:operate 'asdf:load-op system)))
    (try :asdf-system-connections)
    (try :closer-mop)
    (try :cl-syntax-sugar)))

(defpackage :computed-class-system
  (:use :common-lisp
        :asdf
        :closer-mop
        :cl-syntax-sugar)
  (:export
   #:optimize-declaration
   #:*load-with-debug-p*))

(in-package :computed-class-system)

(defparameter *load-with-debug-p* nil)

(defun optimize-declaration ()
  (if *load-with-debug-p*
      '(optimize (debug 3) (safety 3))
      '(optimize (speed 3) (debug 0) (safety 0))))

(defclass computed-class-file (cl-source-file)
  ())

(defmethod perform :around ((op operation) (component computed-class-file))
  (let ((*features* *features*))
    (if *load-with-debug-p*
        (pushnew :debug *features*)
        (pushnew :optimize *features*))
    #+sbcl(pushnew :ensure-method-supports-method-class *features*)
    (call-next-method)))

(defsystem :computed-class
  :version "1.0"
  :author ("Attila Lendvai <attila.lendvai@gmail.com>"
	   "Tamás Borbély <tomi.borbely@gmail.com>"
	   "Levente Mészáros <levente.meszaros@gmail.com>")
  :maintainer ("Attila Lendvai <attila.lendvai@gmail.com>"
               "Tamás Borbély <tomi.borbely@gmail.com>"
	       "Levente Mészáros <levente.meszaros@gmail.com>")
  :licence "BSD"
  :description "Computed class is a class meta object which supports computed slots."
  :default-component-class cl-source-file-with-readtable
  :class system-with-readtable
  :setup-readtable-function "computed-class::setup-readtable"
  :depends-on (:cl-def
               :cl-yalog
               :cl-syntax-sugar
               :anaphora
               :alexandria
               :closer-mop)
  :default-component-class computed-class-file
  :components
  ((:file "package")
   (:file "duplicates" :depends-on ("package"))
   (:file "configuration" :depends-on ("duplicates"))
   (:file "engine" :depends-on ("configuration"))
   (:file "mop" :depends-on ("engine" "configuration"))
   (:file "api" :depends-on ("engine" "mop"))
   (:file "clet" :depends-on ("engine"))
   (:file "defcfun" :depends-on ("engine"))))

(defsystem-connection computed-class-and-swank
  :requires (:computed-class #:cl-syntax-sugar-and-swank)
  :default-component-class cl-source-file-with-readtable
  :class system-connection-with-readtable
  :setup-readtable-function "computed-class::setup-readtable"
  :components
  ((:file "swank-integration")))

(defsystem :computed-class-test
  :description "Tests for the computed-class system."
  :depends-on (:computed-class :stefil)
  :components
  ((:file "test")))

(defmethod perform ((op test-op) (system (eql (find-system :computed-class))))
  (operate 'load-op :computed-class)
  (operate 'load-op :computed-class-test)
  (in-package :computed-class-test)
  (declaim (optimize (debug 3)))
  (warn "(declaim (optimize (debug 3))) was issued to help later C-c C-c'ing")
  (eval (read-from-string "(stefil:funcall-test-with-feedback-message 'test)")))

(defmethod operation-done-p ((op test-op) (system (eql (find-system :computed-class))))
  nil)
