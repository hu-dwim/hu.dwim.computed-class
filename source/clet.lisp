;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.computed-class)

;;;;;;
;;; Standalone variables

(macrolet ((definer (toplevel-name definer-name)
               `(def (macro e) ,toplevel-name (name definition &optional (doc nil doc-p) &environment env)
                 "defcvar and defcparameters are like their cl counterparts with one VERY IMPORT difference: they can only be used as a global, rebinding is not possible!"
                 (assert (symbolp name))
                 (let ((state-variable-name (symbolicate "%" name '#:-state))
                       (state-accessor-name (symbolicate name '#:-state)))
                   (assert (compute-as-form? definition) () "You must specify a compute-as form as definition for defcvar")
                   `(progn
                     (,',definer-name ,state-variable-name
                         (let ((new-state ,(ensure-arguments-for-primitive-compute-as-form
                                            (expand-to-primitive-compute-as-form definition env)
                                            :kind 'variable)))
                           (aprog1
                               (if (boundp ',state-variable-name)
                                   (copy-place-independent-slots-of-computed-state new-state ,state-variable-name)
                                   new-state)
                             (debug-only
                               (setf (cs-place-descriptor it) ',name))))
                       ,@(when doc-p (list doc)))
                     (define-symbol-macro ,name (%computed-state-value ,state-variable-name))
                     (define-symbol-macro ,state-accessor-name (,state-accessor-name))
                     (declaim (inline ,state-accessor-name (setf ,state-accessor-name)))
                     (handler-bind ((style-warning #'muffle-warning))
                       (defun ,state-accessor-name ()
                         ,state-variable-name)
                       (defun (setf ,state-accessor-name) (new-value)
                         (incf-pulse ,state-variable-name)
                         (copy-place-independent-slots-of-computed-state new-value ,state-variable-name)
                         new-value))
                     (values))))))
  (definer defcvar defvar)
  (definer defcparameter defparameter))

(def (macro e) clet (vars &body body &environment env)
  "A let* with extra semantics to handle computed variables. For now see the code and the test file for details.
   Available bindings in the body:
     - NAME-state The place itself that holds the computed state, so you can 
       read or setf closed-over computed variables to new (compute-as ...) forms."
  (let ((state-variables (loop for (name definition) :in vars
                               collect (if (compute-as-form? definition)
                                           (gensym (string name))
                                           nil)))
        (local-computed-state-value (gensym "COMPUTED-STATE-VALUE")))
    (setf vars (loop for (name definition) :in vars
                     collect (list name (if (compute-as-form? definition)
                                            (ensure-arguments-for-primitive-compute-as-form
                                             (expand-to-primitive-compute-as-form definition env)
                                             :kind 'variable)
                                            definition))))
    ;; wrap the global computed-state-value accessors and do some extra work specific to handling variables
    `(locally (declare #+sbcl(sb-ext:muffle-conditions sb-ext:compiler-note))
      ;; let's define local gensymed versions of computed-state-value reader and writer
      (flet ((,local-computed-state-value (computed-state)
               (%computed-state-value computed-state))
             ((setf ,local-computed-state-value) (new-value computed-state)
               (declare #.(optimize-declaration))
               (assert (not (computed-state-p new-value)) ()
                       "You are setting a computed-state into a clet variable, you probably don't want to do that. Hint: (setf foo-state (compute-as 42)).")
               (setf (%computed-state-value computed-state) new-value)))
        (symbol-macrolet (,@(loop for (name definition) :in vars
                                  for var :in state-variables
                                  when var collect (list name `(,local-computed-state-value ,var)))
                          ;; these are NAME-state definitions that expand to flet's that read/write
                          ;; the gensym-ed variables, so through them you can access the actual
                          ;; states directly (e.g. to set state variables captured by various
                          ;; closures)
                          ,@(loop for (name definition) :in vars
                                  for var :in state-variables
                                  for state-name = (symbolicate name '#:-state)
                                  when var collect `(,state-name (,var))))
          ;; define the variables themselves
          (let* ,(loop for (name definition) :in vars
                       for var :in state-variables
                       collect (if var
                                   `(,var (aprog1
                                              ,definition
                                            (assert (eq (cs-kind it) 'variable))
                                            (debug-only
                                              (setf (cs-place-descriptor it) ',name))))
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
                                              (incf-pulse ,var)
                                              (copy-place-independent-slots-of-computed-state new-value ,var)
                                              new-value)))
              (declare #+sbcl(sb-ext:unmuffle-conditions))
              ,@body)))))))
