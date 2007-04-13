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

(defmacro aprog1 (ret &body body)
  `(prog1-bind it ,ret ,@body))

(defmacro prog1-bind (var ret &body body)
  `(let ((,var ,ret))
    ,@body
    ,var))

(defun concatenate-symbol (&rest args)
  "Args are processed as parts of the result symbol with an exception: when a package is encountered then it is stored as the target package at intern."
  (let* ((package nil)
         (symbol-name (string-upcase
                       (with-output-to-string (str)
                         (dolist (arg args)
                           (typecase arg
                             (string (write-string arg str))
                             (package (setf package arg))
                             (symbol (unless package
                                       (setf package (symbol-package arg)))
                                     (write-string (symbol-name arg) str))
                             (integer (write-string (princ-to-string arg) str))
                             (character (write-char arg) str)
                             (t (error "Cannot convert argument ~S to symbol" arg))))))))
    (if package
        (intern symbol-name package)
        (intern symbol-name))))

;; from metabang-bind
(defmacro define-dynamic-context (name direct-slots &key direct-superclasses
                                       export-symbols (class-name name) chain-parents
                                       (create-struct nil) (create-class (not create-struct))
                                       struct-options
                                       (defclass-macro-name 'defclass))
  "This macro generates with-NAME/in-NAME/current-NAME/has-NAME macros to access a CLOS instance in a special variable.
   The purpose is to provide an easy way to access a group of related cotextual values."
  (assert (and (or create-class
                   create-struct
                   (not (or direct-slots direct-superclasses chain-parents)))
               (or (not create-struct)
                   (not direct-superclasses)))
          () "Invalid combination of DIRECT-SLOTS, DIRECT-SUPERCLASSES, CHAIN-PARENTS and CREATE-CLASS/CREATE-STRUCT.")
  (assert (or (not struct-options) create-struct) () "STRUCT-OPTIONS while no CREATE-STRUCT?")
  (assert (not (and create-class create-struct)) () "Only one of CREATE-CLASS and CREATE-STRUCT is allowed.")
  (let ((special-var-name (concatenate-symbol "%" name "%"))
        (extractor-name (concatenate-symbol "current-" name))
        (has-checker-name (concatenate-symbol "has-" name))
        (with-new-macro-name (concatenate-symbol "with-new-" name))
        (with-macro-name (concatenate-symbol "with-" name))
        (struct-constructor-name (when create-struct
                                   (or (second (assoc :constructor struct-options))
                                       (concatenate-symbol "make-" name))))
        (struct-conc-name (when create-struct
                            (or (second (assoc :conc-name struct-options))
                                (concatenate-symbol class-name "-")))))
    `(progn
      ,(when export-symbols
             `(export (list ',extractor-name
                       ',with-new-macro-name
                       ',with-macro-name)))
      ;; generate the context class definition
      ,(when create-class
             `(,defclass-macro-name ,class-name ,direct-superclasses
               ,(if chain-parents
                    (append `((parent-context nil :accessor parent-context-of)) direct-slots) ; accessor is explicitly given to force it to be interned in this package
                    direct-slots)))
      ,(when create-struct
             `(defstruct (,name ,@struct-options)
               ,@(if chain-parents
                     (append `((parent-context nil :type (or null ,class-name))) direct-slots)
                     direct-slots)))
      ;; generate the with-new-... macro
      (defmacro ,with-new-macro-name ((&rest initargs &key &allow-other-keys)
                                      &body forms)
        `(,',with-macro-name ,,(if create-struct
                                   ``(,',struct-constructor-name ,@initargs)
                                   ``(make-instance ',',class-name ,@initargs))
          ,@forms))
      ;; generate the with-... macro
      (defmacro ,with-macro-name (context &body forms)
        (let ((context-instance (gensym "CONTEXT-INSTANCE"))
              (parent (gensym "PARENT")))
          (declare (ignorable parent))
          `(let* ((,context-instance ,context)
                  ,@,(when chain-parents
                           ``((,parent (when (,',has-checker-name)
                                         (,',extractor-name)))))
                  (,',special-var-name ,context-instance))
            (declare (special ,',special-var-name))
            ,@,(when chain-parents
                     ``((setf (,',(if create-struct
                                      (concatenate-symbol struct-conc-name "parent-context")
                                      'parent-context-of) ,context-instance)
                         ,parent)))
            (unless ,context-instance
              (error ,',(strcat "Called with nil " (string-downcase name))))
            ,@forms)))
      ;; generate the in-... macro
      (defmacro ,(concatenate-symbol "in-" name) (var-name-or-slot-name-list &body forms)
        (let ((slots (when (listp var-name-or-slot-name-list)
                       var-name-or-slot-name-list)))
          (if slots
              `(with-slots ,slots (,',extractor-name)
                ,@forms)
              `(let ((,var-name-or-slot-name-list (,',extractor-name)))
                (unless ,var-name-or-slot-name-list
                  (error ,',(strcat "There's no " (string-downcase name))))
                ,@forms))))
      ;; generate the current-... function
      (declaim (inline ,extractor-name))
      (defun ,extractor-name ()
        (symbol-value ',special-var-name))
      ;; generate the has-... function
      (declaim (inline ,has-checker-name))
      (defun ,has-checker-name ()
        (boundp ',special-var-name)))))


;;; from alexandria
(defun parse-body (body &key documentation whole)
  "Parses BODY into (values remaining-forms declarations doc-string).
Documentation strings are recognized only if DOCUMENTATION is true.
Syntax errors in body are signalled and WHOLE is used in the signal
arguments when given."
  (let ((doc nil)
        (decls nil)
        (current nil))
    (tagbody
     :declarations
       (setf current (car body))
       (when (and documentation (stringp current) (cdr body))
         (if doc
             (error "Too many documentation strings in ~S." (or whole body))
             (setf doc (pop body)))
         (go :declarations))
       (when (and (consp current)
                  (eq 'declare (first current)))
         (push (pop body) decls)
         (go :declarations)))
    (values body (nreverse decls) doc)))
