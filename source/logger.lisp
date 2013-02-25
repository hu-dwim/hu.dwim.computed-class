;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.computed-class)

;; KLUDGE this will break if the user first loads :hu.dwim.logger then compiles :hu.dwim.computed-class and then restarts the image
;; and tries to load the :hu.dwim.computed-class fasl's without loading :hu.dwim.logger beforehand.
#*(((find-package :hu.dwim.logger)
    ;; if we are loaded after :hu.dwim.logger, then use a full-featured logger
    (def hu.dwim.logger:logger computed-class () :accessor-name-prefix #:log.))
   (t
    ;; otherwise only a fake minimalistic hu.dwim.logger emulation
    (macrolet ((frob (name)
                `(def macro ,name (message &rest args)
                   (declare (ignore message args))
                   ;; `(format *debug-io* ,(string+ message "~%") ,@args)
                   `(values))))
     (frob log.fatal)
     (frob log.error)
     (frob log.warn)
     (frob log.info)
     (frob log.debug)
     (frob log.dribble))))
