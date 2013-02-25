;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.def)

(def package :hu.dwim.computed-class
  (:use :hu.dwim.asdf
        :hu.dwim.common
        :hu.dwim.def
        :hu.dwim.util
        :hu.dwim.syntax-sugar)
  (:export #:-current-value-)
  (:readtable-setup (hu.dwim.util:enable-standard-hu.dwim-syntaxes)))
