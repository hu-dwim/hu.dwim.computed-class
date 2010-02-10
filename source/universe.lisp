;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.computed-class)

(def class computed-universe-description ()
  ((compute-as-macro-name
    :type symbol
    :initarg :compute-as-macro-name
    :accessor compute-as-macro-name-of)
   (compute-as-macro-name/primitive
    :type symbol
    :initarg :compute-as-macro-name/primitive
    :accessor compute-as-macro-name/primitive-of)
   (default-recomputation-mode
    :initarg :default-recomputation-mode
    :accessor default-recomputation-mode-of)
   (universe-accessor-form
    :initarg :universe-accessor-form
    :accessor universe-accessor-form-of)
   (universe-factory-form
    :initarg :universe-factory-form
    :accessor universe-factory-form-of)))

(def (namespace e) computed-universe (compute-as-macro-name &rest args &key
                                                            default-recomputation-mode
                                                            compute-as-macro-name/primitive
                                                            universe-accessor-form
                                                            universe-factory-form
                                                            self-variable-name
                                                            current-value-variable-name)
  (declare (ignore default-recomputation-mode compute-as-macro-name/primitive universe-accessor-form
                   universe-factory-form self-variable-name current-value-variable-name))
  (apply #'%expand-computed-universe-definition compute-as-macro-name args))

(def (macro e) define-computed-universe (compute-as-macro-name &rest args &key
                                                               default-recomputation-mode
                                                               compute-as-macro-name/primitive
                                                               universe-accessor-form
                                                               universe-factory-form
                                                               self-variable-name
                                                               current-value-variable-name)
  (declare (ignore default-recomputation-mode compute-as-macro-name/primitive universe-accessor-form
                   universe-factory-form self-variable-name current-value-variable-name))
  (apply #'%expand-computed-universe-definition compute-as-macro-name args))

(def function compute-as-macro-name? (symbol)
  (do-computed-universe-namespace (name description (values #f nil))
    (when (or (eq symbol (compute-as-macro-name-of description))
              (eq symbol (compute-as-macro-name/primitive-of description)))
      (return (values #t (compute-as-macro-name/primitive-of description))))))

(def function compute-as-form? (form)
  "To identify forms that create a computed state, IOW the (compute-as ...) forms of computed universes."
  (and (consp form)
       (symbolp (first form))
       (compute-as-macro-name? (first form))))

(def function primitive-compute-as-form? (form)
  "To identify (compute-as* ...) forms, which are the primitive computed state factories of computed universes."
  (bind (((:values compute-as-form? compute-as-macro-name/primitive) (compute-as-form? form)))
    (and compute-as-form?
         (eq (first form) compute-as-macro-name/primitive))))

(def function %expand-computed-universe-definition (compute-as-macro-name &key
                                                                          (default-recomputation-mode :on-demand)
                                                                          (compute-as-macro-name/primitive (symbolicate compute-as-macro-name "*"))
                                                                          (universe-accessor-form `(get ',compute-as-macro-name/primitive 'computed-universe))
                                                                          (universe-factory-form `(make-computed-universe :name ,(fully-qualified-symbol-name compute-as-macro-name)))
                                                                          (self-variable-name '-self-)
                                                                          (current-value-variable-name '-current-value-))
  ;; multiple evaluation of DEFAULT-RECOMPUTATION-MODE, avoiding would break toplevelness
  (check-type compute-as-macro-name symbol)
  (bind ((docstring "Generated by a computed universe. It's a macro to instantiate computations that can be stored in computed places, e.g. in a computed slot."))
    `(progn
       (eval-always
         (setf (find-computed-universe ',compute-as-macro-name) (make-instance 'computed-universe-description
                                                                               :compute-as-macro-name ',compute-as-macro-name
                                                                               :compute-as-macro-name/primitive ',compute-as-macro-name/primitive
                                                                               :default-recomputation-mode ,default-recomputation-mode
                                                                               :universe-accessor-form ',universe-accessor-form
                                                                               :universe-factory-form ',universe-factory-form)))

       (defmacro ,compute-as-macro-name/primitive ((&key (kind 'object-slot) (recomputation-mode ',default-recomputation-mode)) &body form)
         ,docstring
         (bind ((self-variable-name ',self-variable-name))
           (unless (eq kind 'object-slot)
             (setf self-variable-name (gensym)))
           (with-unique-names (universe)
             `(bind ((,universe ,',universe-accessor-form))
                (unless ,universe
                  ;; this is not thread-safe here, it's the user's responsibility to make sure of proper thread exclusion
                  (setf ,universe ,',universe-factory-form)
                  (setf (cu-description ,universe) (find-computed-universe ',',compute-as-macro-name :otherwise :error))
                  (setf ,',universe-accessor-form ,universe))
                (check-type ,universe computed-universe)
                (assert (cu-description ,universe))
                (make-computed-state :universe ,universe
                                     :recomputation-mode ',recomputation-mode
                                     #+debug :form #+debug ',form
                                     :compute-as (named-lambda ,',(symbolicate compute-as-macro-name/primitive '#:/a-computation-body)
                                                     (,self-variable-name ,',current-value-variable-name)
                                                   (declare (ignorable ,self-variable-name
                                                                       ,',current-value-variable-name))
                                                   ,@form)
                                     :kind ',kind)))))

       (defmacro ,compute-as-macro-name (&body body)
         ,docstring
         `(,',compute-as-macro-name/primitive () ,@body)))))