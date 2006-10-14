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

;;; THE CONTENT OF THIS FILE IS COPIED OVER FROM SOME OTHER LIBRARIES TO DECREASE DEPENDENCIES

(defmacro enable-sharp-boolean-syntax ()
  "Copies *readtable* and enables #t and #f readers for t and nil in the copy."
  '(eval-when (:compile-toplevel :execute)
    (setf *readtable* (copy-readtable *readtable*))
    (%enable-sharp-boolean-syntax)))

(defun %enable-sharp-boolean-syntax ()
  (set-dispatch-macro-character
   #\# #\t
   (lambda (s c n)
     (declare (ignore s c n))
     t))
  (set-dispatch-macro-character
   #\# #\f
   (lambda (s c n)
     (declare (ignore s c n))
     nil)))

(defun concatenate-symbol (&rest args)
  (let* ((package nil)
         (symbol-name (with-output-to-string (str)
                                             (loop for arg in args do
                                                   (cond ((stringp arg) (format str arg))
                                                         ((packagep arg) (setf package arg))
                                                         ((symbolp arg)
                                                          (unless package
                                                            (setf package (symbol-package arg)))
                                                          (format str (symbol-name arg)))
                                                         (t (error "Cannot convert argument to symbol")))))))
    (intern (string-upcase symbol-name) package)))

(defmacro define-dynamic-context (name direct-slots &key export supers class-name chain-parents)
  "This macro generates a with-name/in-name/current-name triplet.
   The usual context stuff in a special variable..."
  (let ((special-var-name (concatenate-symbol "%" name "%"))
        (current-extractor-function-name (concatenate-symbol "current-" name))
        (has-checker-function-name (concatenate-symbol "has-" name))
        (with-new-macro-name (concatenate-symbol "with-new-" name))
        (with-macro-name (concatenate-symbol "with-" name))
        (name-in-message (string-downcase (symbol-name name))))
    (unless class-name
      (setf class-name name))
    `(progn
      ,@(if export
            `((export (list ',current-extractor-function-name
                       ',with-new-macro-name
                       ',with-macro-name))))
      ;; generate the context class definition
      (defclass ,class-name ,supers
        ,(if chain-parents
             (append `((parent-context
                        :initform nil
                        :accessor parent-context-of)) direct-slots)
             direct-slots))
      ;; generate the with-new-... macro
      (defmacro ,with-new-macro-name ((&rest initargs &key &allow-other-keys)
                                      &body forms)
        `(let ((,',special-var-name (make-instance ',',name ,@initargs)))
          (declare (special ,',special-var-name))
          ,@forms))
      ;; generate the with-... macro
      (defmacro ,with-macro-name (context &body forms)
        (with-unique-names (context-instance parent)
          (declare (ignorable parent))
          `(let* ((,context-instance ,context)
                  ,@,(when chain-parents
                           ``((,parent (when (,',has-checker-function-name)
                                         (,',current-extractor-function-name)))))
                  (,',special-var-name ,context-instance))
            (declare (special ,',special-var-name))
            ,@,(when chain-parents
                     ``((setf (parent-context-of ,context-instance) ,parent)))
            (unless ,context-instance
              (error ,,(strcat "Called with nil " name-in-message)))
            ,@forms)))
      ;; generate the in-... macro
      (defmacro ,(concatenate-symbol "in-" name) (var-name-or-slot-name-list &body forms)
        (let ((slots (when (listp var-name-or-slot-name-list)
                       var-name-or-slot-name-list)))
          (if slots
              `(with-slots ,slots (,',current-extractor-function-name)
                ,@forms)
              `(let ((,var-name-or-slot-name-list (,',current-extractor-function-name)))
                (unless ,var-name-or-slot-name-list
                  (error ,,(strcat "There's no " name-in-message)))
                ,@forms))))
      ;; generate the current-... function
      (defun ,current-extractor-function-name ()
        (declare (inline) (special ,special-var-name))
        ,special-var-name)
      ;; generate the has-... function
      (defun ,has-checker-function-name ()
        (declare (inline))
        (boundp ',special-var-name)))))
