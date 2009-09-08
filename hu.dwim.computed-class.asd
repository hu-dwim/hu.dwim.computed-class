;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.computed-class
  :class hu.dwim.system
  :author ("Attila Lendvai <attila.lendvai@gmail.com>"
           "Tamás Borbély <tomi.borbely@gmail.com>"
           "Levente Mészáros <levente.meszaros@gmail.com>")
  :licence "BSD / Public domain"
  :description "Constraint based change propagation"
  :depends-on (:hu.dwim.common-lisp
               :hu.dwim.def
               :hu.dwim.logger
               :hu.dwim.syntax-sugar)
  :components ((:module "source"
                :components ((:file "package")
                             (:file "duplicates" :depends-on ("package"))
                             (:file "configuration" :depends-on ("duplicates"))
                             (:file "engine" :depends-on ("configuration"))
                             (:file "mop" :depends-on ("engine" "configuration"))
                             (:file "api" :depends-on ("engine" "mop"))
                             (:file "clet" :depends-on ("engine"))
                             (:file "defcfun" :depends-on ("engine"))))))
