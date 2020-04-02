;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.computed-class)

(def function %defcclass/body (definer-macro-name name superclasses slots options)
  `(,definer-macro-name ,name ,superclasses
     ,slots
     ,@(append (unless (find :metaclass options :key 'first)
                 '((:metaclass computed-class)))
               options)))

(def (macro e) defcclass* (name superclasses slots &rest options)
  (%defcclass/body 'hu.dwim.defclass-star:defclass* name superclasses slots options))

(def (definer e :available-flags "e") computed-class* (name superclasses slots &rest options)
  `(progn
     (defcclass* ,name ,superclasses ,slots ,@options)
     ,@(when (getf -options- :export)
         `((export ',name)))))

(mapc (lambda (option)
        (pushnew option hu.dwim.defclass-star:*allowed-slot-definition-properties*))
      '(:computed-in :compute-as :slot-value-function :setf-slot-value-function))
