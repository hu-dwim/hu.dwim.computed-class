;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.computed-class.documentation)

(def project :hu.dwim.computed-class)

(def book user-guide (:title "User guide")
  (chapter (:title "Introduction")
    (paragraph ()
      "TODO"))
  (chapter (:title "Supported Common Lisp Implementations")
    (paragraph ()
      "SBCL"))
  (chapter (:title "Supported Operating Systems")
    (paragraph ()
      "Linux"))
  (chapter (:title "Tutorial")
    (paragraph ()
      "TODO")))

;; TODO:

#|
Constraint based change propagation, with special care taken for CLOS MOP integation

partial lists follow...

features:
 - the method based slot access is only a bit slower than the standard one when there's no computation to be done
 - support for separate computed universes
 - support for both using reified computed cells, and also clet and computed slots



limitations:

 - no forward propagation, only on demand forcing of computation
 - most probably uses more memory than necessary
 - iirc, the optimized slot accessor generation is flanky, could use someone with the required MOP background
 - integrating with other MOP's could be better supported with an exported svuc-body macro providing various hooks
|#
