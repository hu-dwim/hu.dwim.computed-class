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

(macrolet ((definer (name definer-name)
               `(defmacro ,name (name definition &optional (doc nil doc-p) &environment env)
                 "defcvar and defcparameters are like their cl counterparts with one VERY IMPORT difference: they can only be used as a global, rebinding is not possible!"
                 (let ((state-variable-name (concatenate-symbol "%" name "-state"))
                       (state-accessor-name (concatenate-symbol name "-state")))
                   (assert (compute-as-form-p definition) () "You must specify a compute-as form as definition for defcvar")
                   `(progn
                     ,,(when (eq definer-name 'defparameter)
                             ``(when (and (boundp ',state-variable-name)
                                      (computed-state-p ,state-variable-name))
                                (incf-pulse ,state-variable-name)
                                (setf (cs-attached-p ,state-variable-name) #f)))
                     (,',definer-name ,state-variable-name
                         (aprog1
                             ,(ensure-arguments-for-primitive-compute-as-form
                               (primitive-compute-as-form-of definition env)
                               :kind 'variable)
                           (setf (cs-attached-p it) #t)
                           (setf (cs-variable it) ',name))
                       ,@(when doc-p (list doc)))
                     (define-symbol-macro ,name (computed-state-value ,state-variable-name))
                     (define-symbol-macro ,state-accessor-name (,state-accessor-name))
                     (declaim (inline ,state-accessor-name (setf ,state-accessor-name)))
                     (handler-bind ((style-warning #'muffle-warning))
                       (defun ,state-accessor-name ()
                         ,state-variable-name)
                       (defun (setf ,state-accessor-name) (new-value)
                         (incf-pulse ,state-variable-name)
                         (setf (cs-attached-p ,state-variable-name) #f)
                         (setf (cs-attached-p new-value) #t)
                         (setf ,state-variable-name new-value)))
                     ',name)))))
  (definer defcvar defvar)
  (definer defcparameter defparameter))

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
                                            (ensure-arguments-for-primitive-compute-as-form
                                             (primitive-compute-as-form-of definition env)
                                             :kind 'variable)
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
                          ;; these are NAME-state definitions that expand to flet's that read/write
                          ;; the gensym-ed variables, so through them you can access the actual
                          ;; states directly (e.g. to set state variables captured by various
                          ;; closures)
                          ,@(loop for (name definition) :in vars
                                  for var :in state-variables
                                  for state-name = (concatenate-symbol name "-state")
                                  when var collect `(,state-name (,var))))
          ;; define the variables themselves
          (let* ,(loop for (name definition) :in vars
                       for var :in state-variables
                       collect (if var
                                   `(,var (aprog1
                                              ,definition
                                            (assert (eq (cs-kind it) 'variable))
                                            (setf (cs-attached-p it) #t)
                                            (setf (cs-variable it) ',name)))
                                   (list name definition)))
            (declare (ignorable ,@(remove-if #'null state-variables)))
            ;; define reader and writer flet's named by the gensym-ed variables. they will handle the read/write of the states
            (flet (,@(loop for (name nil) :in vars
                           for var :in state-variables
                           when var collect `(,var ()
                                              ,var))
                   ,@(loop for (name nil) :in vars
                           for var :in state-variables
                           when var collect `((setf ,var) (new-value)
                                              (incf-pulse new-value)
                                              (setf (cs-attached-p ,var) #f)
                                              (setf (cs-attached-p new-value) #t)
                                              (setf ,var new-value))))
              (declare #+sbcl(sb-ext:unmuffle-conditions))
              ,@body)))))))



