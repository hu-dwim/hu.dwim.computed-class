;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.computed-class
  :class hu.dwim.system
  :description "Constraint based change propagation for class slots, lexical variables, function return values and reified cells."
  :depends-on (:hu.dwim.def+hu.dwim.common
               :hu.dwim.logger
               :hu.dwim.syntax-sugar
               :hu.dwim.util.mop)
  :components ((:module "source"
                :components ((:file "api" :depends-on ("engine" "mop"))
                             (:file "clet" :depends-on ("engine"))
                             (:file "util" :depends-on ("duplicates" "logger"))
                             (:file "defcfun" :depends-on ("engine"))
                             (:file "engine" :depends-on ("util"))
                             (:file "logger" :depends-on ("package"))
                             (:file "mop" :depends-on ("engine"))
                             (:file "package")))))
