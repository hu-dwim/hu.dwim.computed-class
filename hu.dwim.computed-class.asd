;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(defsystem :hu.dwim.computed-class
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.system"
  :description "Constraint based change propagation for class slots, lexical variables, function return values and reified cells."
  :depends-on (:hu.dwim.def+hu.dwim.common
               :hu.dwim.syntax-sugar
               :hu.dwim.defclass-star+hu.dwim.def ;; also brought in by :hu.dwim.util.mop
               :hu.dwim.util.mop)
  :components ((:module "source"
                :components ((:file "api" :depends-on ("package"))
                             (:file "clet" :depends-on ("engine"))
                             (:file "clos" :depends-on ("clos-mop" "engine"))
                             (:file "clos-mop" :depends-on ("engine"))
                             (:file "defcfun" :depends-on ("engine"))
                             (:file "defclass-star" :depends-on ("package"))
                             (:file "engine" :depends-on ("api" "universe" "util"))
                             (:file "logger" :depends-on ("package"))
                             (:file "package")
                             (:file "sequence" :depends-on ("api"))
                             (:file "universe" :depends-on ("api"))
                             (:file "util" :depends-on ("logger"))))))
