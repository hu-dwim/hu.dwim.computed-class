;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2013 by the authors.
;;;
;;; See LICENCE for details.

(defsystem :hu.dwim.computed-class+hu.dwim.logger
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.system"
  :depends-on (:hu.dwim.computed-class
               :hu.dwim.logger))
