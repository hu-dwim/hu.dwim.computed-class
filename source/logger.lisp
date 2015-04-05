;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.computed-class)

;; this mess here is for having fewer dependencies. if you need logging for debugging, then edit the read-time condition below and recompile.
#*((nil
    ;; if we are loaded after :hu.dwim.logger, then use a full-featured logger
    (def hu.dwim.logger:logger computed-class () :accessor-name-prefix #:log.))
   (t
    ;; otherwise only a fake minimalistic hu.dwim.logger emulation
    (macrolet ((frob (name)
               `(progn
                  ,@(loop
                      :for postfix :in '(#:fatal #:error #:warn #:info #:debug #:dribble)
                      :collect
                      `(def macro ,(symbolicate name "." postfix) (message &rest args)
                         (declare (ignore message args))
                         ;; `(format *debug-io* ,(string+ message "~%") ,@args)
                         `(values))))))
     (frob log))))
