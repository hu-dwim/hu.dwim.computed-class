;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(defsystem :hu.dwim.computed-class+hu.dwim.defclass-star
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.system"
  :depends-on (:hu.dwim.computed-class
               :hu.dwim.defclass-star)
  :components ((:module "integration"
                :components ((:file "defclass-star")))))
