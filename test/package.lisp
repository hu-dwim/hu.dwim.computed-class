;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.def)

(def package :hu.dwim.computed-class.test
  (:use :hu.dwim.asdf
        :hu.dwim.common
        :hu.dwim.computed-class
        :hu.dwim.def
        :hu.dwim.logger
        :hu.dwim.stefil
        :hu.dwim.util)
  (:readtable-setup (setup-readtable/same-as-package :hu.dwim.computed-class)))

(in-package :hu.dwim.computed-class)

(eval-always
  (import '(computed-state-or-nil computed-effective-slot-definition current-pulse
            slot-value-using-class-body setf-slot-value-using-class-body
            standard-instance-access-form computed-state-p
            log.dribble log.debug log.info log.warn log.error
            cs-kind cs-place-descriptor cs-depends-on)
          (find-package :hu.dwim.computed-class.test)))
