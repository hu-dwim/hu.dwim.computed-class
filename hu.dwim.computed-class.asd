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
  :depends-on (:hu.dwim.def+hu.dwim.common
               :hu.dwim.logger
               :hu.dwim.syntax-sugar
               :hu.dwim.util.mop)
  :components ((:module "source"
                :components ((:file "api" :depends-on ("engine" "mop"))
                             (:file "clet" :depends-on ("engine"))
                             (:file "util" :depends-on ("duplicates" "logger"))
                             (:file "defcfun" :depends-on ("engine"))
                             (:file "duplicates" :depends-on ("package"))
                             (:file "engine" :depends-on ("util"))
                             (:file "logger" :depends-on ("package"))
                             (:file "mop" :depends-on ("engine"))
                             (:file "package")))))
