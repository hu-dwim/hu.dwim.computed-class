;; -*- mode: Lisp; Syntax: Common-Lisp; Package: computed-class; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a copy 
;;; of this software and associated documentation files (the "Software"), to deal 
;;; in the Software without restriction, including without limitation the rights 
;;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell 
;;; copies of the Software, and to permit persons to whom the Software is furnished 
;;; to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in 
;;; all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR 
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE 
;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER 
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING 
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS 
;;; IN THE SOFTWARE.

(in-package :computed-class)

(enable-sharp-boolean-syntax)

;;;;;;;;;;;;;;;;;;;;;;;;
;;; Standalone variables

;; limitations:
;; - computed-let can only handle variables initialized with (compute-as ...) forms (can be fixed)
;; - (computed-state-for NAME) works only on one level (probably not trivial to overcome this)
;; - warnings/notes due to unused functions and optimization

(defmacro clet (vars &body body)
  "A let* with extra semantics to handle computed variables. For now see the code and the test file for details.
   Available bindings in the body:
     - (computed-state-for NAME) A macro to access the place itself, so you can setf closed-over computed variables to new (compute-as ...) forms."
  (let ((state-variables (loop for (name nil) :in vars
                               collect (gensym (string name)))))
    ;; wrap the global computed-state-value accessors and do some extra work specific to handling variables
    `(flet ((computed-state-value (computed-state)
             (declare #.(optimize-declaration))
             (when (has-recompute-state-contex)
               (in-recompute-state-contex context
                 (push computed-state (rsc-used-computed-states context))))
             (computed-state-value computed-state))
           ((setf computed-state-value) (new-value computed-state)
             (declare #.(optimize-declaration))
             #+debug(assert (not (typep new-value 'computed-state)) () "Setting computed-state's into variables should be done through the (computed-state-for var-name) form")
             (setf (computed-state-value computed-state) new-value)))
      (symbol-macrolet (,@(loop for (name definition) :in vars
                                for var = (gensym (string name))
                                collect var :into vars
                                collect (list name `(computed-state-value ,var)) :into result
                                finally (progn
                                          (setf state-variables vars)
                                          (return result)))
                          ;; these are NAME-state definitions that expand to the gensym-ed variables,
                          ;; so through them you can access the actual states directly (e.g. to set
                          ;; state variables captured by various closures) 
                          ,@(loop for (name definition) :in vars
                                  for var :in state-variables
                                  for state-name = (concatenate-symbol name "-state")
                                  collect (list state-name var)))
        (macrolet ((computed-state-for (variable)
                     (case variable
                       ,@(loop for (name nil) :in vars
                               for state-variable :in state-variables
                               collect `(,name ',state-variable))
                       (t (error "Limitation: computed-state-for can only access the state variables in the closest computed-let form. Variable ~S was not found there." variable)))))
          (let* ,(loop for (name definition) :in vars
                       for var :in state-variables
                       collect (list var definition))
            (declare (ignorable ,@state-variables))
            ,@body))))))



