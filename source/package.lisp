;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.util)

(def package :hu.dwim.computed-class
  (:use :hu.dwim.asdf
        :hu.dwim.common
        :hu.dwim.def
        :hu.dwim.util
        :hu.dwim.logger
        :hu.dwim.syntax-sugar)
  (:export #:defcclass
           #:computed-class
           #:computed-object
           #:computed-direct-slot-definition
           #:computed-effective-slot-definition
           #:computed-class*
           #:define-computed-universe
           #:make-computed-universe
           #:-current-value-
           #:make-slot-uncomputed
           #:clet
           #:defcvar
           #:defcparameter
           #:defcfun
           #:computed-state-value
           #:computation-of-computed-state)
  ;; for debug purposes
  (:export #:computed-state-for
           #:invalidate-computed-slot
           #:invalidate-computed-state
           #:computed-slot-valid-p
           #:recompute-slot
           #:recompute-computed-state)
  (:readtable-setup (enable-standard-hu.dwim-syntaxes)))
