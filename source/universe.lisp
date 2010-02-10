;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.computed-class)

(def class computed-universe-class (standard-class)
  ((computed-state-factory-name
    :type symbol
    :initarg :computed-state-factory-name
    :reader computed-state-factory-name-of)
   (computed-state-factory-name/primitive
    :type symbol
    :initarg :computed-state-factory-name/primitive
    :reader computed-state-factory-name/primitive-of)
   (default-recomputation-mode
    :initarg :default-recomputation-mode
    :reader default-recomputation-mode-of)
   (universe-accessor-form
    :initarg :universe-accessor-form
    :reader universe-accessor-form-of)
   (universe-factory-form
    :initarg :universe-factory-form
    :reader universe-factory-form-of)))

(def method validate-superclass ((subclass computed-universe-class) (superclass standard-class))
  (subtypep (class-of subclass) (class-of superclass)))

(def class computed-universe ()
  ((pulse
    :initform 0
    :type integer
    :initarg :pulse
    :accessor pulse-of
    :documentation "This counter will be incremented each time a computed-state is set externally. So, when a computed-state is recomputed (e.g. due to changes in the computed-states that were read while computing the previous value), then this counter will not change. The first valid pulse value is 0."))
  (:metaclass computed-universe-class))

(def (definer e) computed-universe (name &rest args &key
                                         computed-state-factory-name
                                         default-recomputation-mode
                                         computed-state-factory-name/primitive
                                         universe-accessor-form
                                         universe-factory-form
                                         self-variable-name
                                         current-value-variable-name)
  (declare (ignore computed-state-factory-name default-recomputation-mode computed-state-factory-name/primitive universe-accessor-form
                   universe-factory-form self-variable-name current-value-variable-name))
  (apply #'%expand-computed-universe-definition name args))

(def (macro e) define-computed-universe (name &rest args &key
                                              computed-state-factory-name
                                              default-recomputation-mode
                                              computed-state-factory-name/primitive
                                              universe-accessor-form
                                              universe-factory-form
                                              self-variable-name
                                              current-value-variable-name)
  (declare (ignore computed-state-factory-name default-recomputation-mode computed-state-factory-name/primitive universe-accessor-form
                   universe-factory-form self-variable-name current-value-variable-name))
  (apply #'%expand-computed-universe-definition name args))

(def function computed-state-factory-name? (symbol)
  (block nil
    (bind ((universe-name (get symbol 'computed-state-factory-of)))
      (unless universe-name
        (return #f))
      (bind ((universe (find-computed-universe universe-name :otherwise nil)))
        (unless universe
          (return #f))
        (values #t (computed-state-factory-name/primitive-of universe) universe)))))

(def function find-computed-universe (name &key (otherwise :error))
  (check-type name symbol)
  (bind ((universe (find-class name nil)))
    (if (and universe
             (typep universe 'computed-universe-class))
        universe
        (handle-otherwise otherwise))))

(def function compute-as-form? (form)
  "To identify forms that create a computed state, IOW the (compute-as ...) forms of computed universes."
  (and (consp form)
       (symbolp (first form))
       (computed-state-factory-name? (first form))))

(def function primitive-compute-as-form? (form)
  "To identify (compute-as* ...) forms, which are the primitive computed state factories of computed universes."
  (bind (((:values compute-as-form? computed-state-factory-name/primitive) (compute-as-form? form)))
    (and compute-as-form?
         (eq (first form) computed-state-factory-name/primitive))))

(def function %expand-computed-universe-definition (name &key
                                                         (computed-state-factory-name (symbolicate name '#:/compute-as))
                                                         (computed-state-factory-name/primitive (symbolicate computed-state-factory-name "*"))
                                                         (default-recomputation-mode :on-demand)
                                                         (universe-accessor-form `(get ',name 'computed-universe-instance))
                                                         (universe-factory-form `(make-instance ',name))
                                                         (self-variable-name '-self-)
                                                         (current-value-variable-name '-current-value-))
  ;; multiple evaluation of DEFAULT-RECOMPUTATION-MODE, avoiding would break toplevelness
  (check-type computed-state-factory-name symbol)
  (bind ((docstring "Generated by a computed universe. It's a macro to instantiate computations that can be stored in computed places, e.g. in a computed slot.")
         (metaclass-name (symbolicate name '#:/metaclass)))
    `(progn
       (eval-always
         (setf (get ',computed-state-factory-name           'computed-state-factory-of) ',name)
         (setf (get ',computed-state-factory-name/primitive 'computed-state-factory-of) ',name)
         (def class ,metaclass-name (computed-universe-class)
           ()
           (:default-initargs
               :computed-state-factory-name ',computed-state-factory-name
             :computed-state-factory-name/primitive ',computed-state-factory-name/primitive
             :default-recomputation-mode ,default-recomputation-mode
             :universe-accessor-form ',universe-accessor-form
             :universe-factory-form ',universe-factory-form))
         (defclass ,name (computed-universe)
           ()
           (:metaclass ,metaclass-name)))
       (defmacro ,computed-state-factory-name/primitive ((&key (kind 'object-slot) (recomputation-mode ',default-recomputation-mode)) &body form)
         ,docstring
         (bind ((self-variable-name ',self-variable-name))
           (unless (eq kind 'object-slot)
             (setf self-variable-name (gensym)))
           (with-unique-names (universe)
             `(bind ((,universe ,',universe-accessor-form))
                (unless ,universe
                  ;; this is not thread-safe here, it's the user's responsibility to make sure of proper thread exclusion
                  (setf ,universe ,',universe-factory-form)
                  (setf ,',universe-accessor-form ,universe))
                (check-type ,universe computed-universe)
                (make-computed-state :universe ,universe
                                     :recomputation-mode ',recomputation-mode
                                     #+debug :form #+debug ',form
                                     :compute-as (named-lambda ,',(symbolicate computed-state-factory-name/primitive '#:/a-computation-body)
                                                     (,self-variable-name ,',current-value-variable-name)
                                                   (declare (ignorable ,self-variable-name
                                                                       ,',current-value-variable-name))
                                                   ,@form)
                                     :kind ',kind)))))
       (defmacro ,computed-state-factory-name (&body body)
         ,docstring
         `(,',computed-state-factory-name/primitive () ,@body)))))
