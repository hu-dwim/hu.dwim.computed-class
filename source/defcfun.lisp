;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.computed-class)

(def (macro e) defcfun (name args &body body)
  "Just like a defun, but assumes that the code executed in its body does not have any sideeffects and based on this assumption memoizes the computed return values. The memoized entries are dropped if any computed-state is invalidated that was read while calculating the memoize entry in question."
  (bind (((name &key (memoize-test-fn 'equal) computed-in maximum-cache-entries (hash-table-factory 'make-hash-table))
          (ensure-list name)))
    (multiple-value-bind (body declarations documentation) (parse-body body :documentation t)
      (with-unique-names (memoize-table memoize-key state)
        (bind ((universe (find-computed-universe computed-in :otherwise `(:error "The specified :computed-in argument ~S is not a compute-as macro in any computed universe" ,computed-in)))
               (&rest-name nil))
          (multiple-value-setq (args &rest-name) (ensure-&rest-in-lambda-list args))
          `(progn
            (awhen (get ',name 'memoize-table)
              ;; seems like we are redefining this defcfun, so invalidate the old cached states
              (loop for value being the hash-values of it do
                    (invalidate-computed-state value)))
            (let ((,memoize-table (funcall ',hash-table-factory :test ',memoize-test-fn)))
              (setf (get ',name 'memoize-table) ,memoize-table)
              (values (defun ,name ,args
                        ,@(awhen documentation (list it))
                        ,@declarations
                        (let* ((,memoize-key (list* ,@(subseq args 0 (position-if (rcurry #'member '(&rest &key)) args))
                                                    ,&rest-name))
                               (,state (gethash ,memoize-key ,memoize-table)))
                          (unless ,state
                            (setf ,state (,(computed-state-factory-name/primitive-of universe) (:kind standalone)
                                           (multiple-value-list (progn ,@body))))
                            ,(when maximum-cache-entries
                                   `(when (> (hash-table-count ,memoize-table) ,maximum-cache-entries)
                                     ;; PUNT: simply drop the entire cache when we go over the
                                     ;; maximum-cache-entries limit
                                     (loop for value being the hash-values of it do
                                           (invalidate-computed-state value))
                                     (clrhash ,memoize-table)))
                            (setf (gethash ,memoize-key ,memoize-table) ,state))
                          (values-list (computed-state-value ,state))))
                      ,memoize-table))))))))

(def function ensure-&rest-in-lambda-list (lambda-list)
  (let ((&rest-name nil))
    (values (loop for cell = lambda-list :then (cdr cell)
                  while cell
                  for el = (car cell)
                  append (cond ((eq el '&rest)
                                (setf &rest-name (cadr cell))
                                (list el))
                               ((eq el '&key)
                                (if &rest-name
                                    (list el)
                                    (progn
                                      (setf &rest-name (gensym "REST-ARGS"))
                                      `(&rest ,&rest-name &key))))
                               (t (list el))))
            &rest-name)))
