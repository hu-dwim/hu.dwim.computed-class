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

#.(file-header)

;;;;;;;;;;;;;;;;;;;;;;;;
;;; Standalone variables

;; current limitations:
;; - computed-let can only handle variables initialized with (compute-as ...) forms (can be fixed)
;; - (computed-state-for NAME) works only on one level (probably not trivial to overcome this)
;; - warnings/notes due to unused functions and optimization

(defmacro clet (vars &body body &environment env)
  "A let* with extra semantics to handle computed variables. For now see the code and the test file for details.
   Available bindings in the body:
     - (computed-state-for NAME) A macro to access the place itself that holds the computed state, so you can 
       setf closed-over computed variables to new (compute-as ...) forms."
  (let ((state-variables (loop for (name definition) :in vars
                               collect (if (compute-as-form-p definition)
                                           (gensym (string name))
                                           nil))))
    (setf vars (loop for (name definition) :in vars
                     collect (list name (if (compute-as-form-p definition)
                                            (primitive-compute-as-form-with-ensured-kind (primitive-compute-as-form-of definition env)
                                                                                         'variable)
                                            definition))))
    ;; wrap the global computed-state-value accessors and do some extra work specific to handling variables
    `(locally (declare #+sbcl(sb-ext:muffle-conditions sb-ext:compiler-note))
      (flet (((setf computed-state-value) (new-value computed-state)
               (declare #.(optimize-declaration))
               (assert (not (computed-state-p new-value)) ()
                       "You are setting a computed-state into a clet variable, you probably don't want to do that. Hint: (setf foo-state (compute-as 42)).")
               (setf (computed-state-value computed-state) new-value)))
        (symbol-macrolet (,@(loop for (name definition) :in vars
                                  for var :in state-variables
                                  when var collect (list name `(computed-state-value ,var)))
                          ;; these are NAME-state definitions that expand to the gensym-ed variables,
                          ;; so through them you can access the actual states directly (e.g. to set
                          ;; state variables captured by various closures) 
                          ,@(loop for (name definition) :in vars
                                  for state-name = (concatenate-symbol name "-state")
                                  collect `(,state-name (,(concatenate-symbol "state-of-" name)))))
          ;; define macrolet's with the name NAME-computed-state that can access the
          (let* ,(loop for (name definition) :in vars
                       for var :in state-variables
                       collect (if var
                                   (list var definition)
                                   (list name definition)))
            (declare (ignorable ,@(remove-if #'null state-variables)))
            (flet (,@(loop for (name nil) :in vars
                           for var :in state-variables
                           when var collect `(,(concatenate-symbol "state-of-" name) ()
                                              ,var))
                   ,@(loop for (name nil) :in vars
                           for var :in state-variables
                           when var collect `((setf ,(concatenate-symbol "state-of-" name)) (new-value)
                                              (incf-pulse new-value)
                                              (setf ,var new-value))))
              (declare #+sbcl(sb-ext:unmuffle-conditions))
              ,@body)))))))



