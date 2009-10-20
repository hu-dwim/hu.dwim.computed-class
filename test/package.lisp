;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.computed-class)

(defpackage :hu.dwim.computed-class.test
  (:use :hu.dwim.common
        :hu.dwim.computed-class
        :hu.dwim.def
        :hu.dwim.stefil))

(eval-always
  (import '(find-slot computed-state-or-nil computed-effective-slot-definition current-pulse
            slot-value-using-class-body setf-slot-value-using-class-body
            enable-sharp-boolean-syntax standard-instance-access-form computed-state-p
            computed-class.dribble computed-class.debug computed-class.info computed-class.warn computed-class.error
            cs-kind cs-variable cs-depends-on)
          (find-package :hu.dwim.computed-class.test)))
