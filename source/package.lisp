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
  (:export #:make-computed-universe
           #:-current-value-)
  (:readtable-setup (enable-standard-hu.dwim-syntaxes)))
