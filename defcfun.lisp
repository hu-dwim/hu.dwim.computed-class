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

(defmacro defcfun (name args &body body)
  "Just like a defun, but assumes that the code executed in its body does not have any sideeffects and based on this assumption memoizes the computed return values. The memoized entries are dropped if any computed-state is invalidated that was read while calculating the memoize entry in question."
  (destructuring-bind (name &key (memoize-test-fn 'equal) computed-in (maximum-cache-entries 10000)
                            (hash-table-factory 'make-hash-table))
      (ensure-list name)
    (assert computed-in (computed-in) "You need to specify with :computed-in which computed universe defcfun's are computed in.")
    (multiple-value-bind (body declarations documentation) (parse-body body :documentation t)
      (with-unique-names (memoize-table memoize-key state)
        (let* ((&rest-name nil)
               (primitive-compute-as-macro-name (get computed-in 'primitive-compute-as-macro)))
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
                            (setf ,state (,primitive-compute-as-macro-name (:kind standalone)
                                                                           (multiple-value-list (progn ,@body))))
                            (when (> (hash-table-count ,memoize-table) ,maximum-cache-entries)
                              (clrhash ,memoize-table))
                            (setf (gethash ,memoize-key ,memoize-table) ,state))
                          (values-list (computed-state-value ,state))))
                      ,memoize-table))))))))

(defun ensure-&rest-in-lambda-list (lambda-list)
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

