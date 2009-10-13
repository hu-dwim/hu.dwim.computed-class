;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.computed-class)

(def logger computed-class ()
  :runtime-level +warn+
  :compile-time-level #+optimize +warn+ #-optimize +dribble+
  :appender (make-instance 'brief-stream-appender :stream *debug-io*))
