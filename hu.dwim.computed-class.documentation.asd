;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.computed-class.documentation
  :class hu.dwim.documentation-system
  :depends-on (:hu.dwim.computed-class.test
               :hu.dwim.wui)
  :components ((:module "documentation"
                :components ((:file "computed-class" :depends-on ("package"))
                             (:file "package")))))
