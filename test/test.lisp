;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.computed-class.test)

(def suite* (test :in root-suite))

;; the two ways to define a computed-universe are equivalent
(def (computed-universe e) test-universe ()
  ()
  (:computed-state-factory-name compute-as))

(define-computed-universe separate-universe ()
  ()
  (:computed-state-factory-name separated-compute-as))

;;;;;;
;;; defclass tests

(def test defclass1 ()
  (finishes
    (defclass computed-1 ()
      ()
      (:metaclass computed-class))
    (defclass computed-2 ()
      ((slot-a
        :initform (compute-as 1)))
      (:metaclass computed-class))
    (let ((class
           (defclass computed-3 ()
             ((a)
              #+sbcl(b :computed-in nil)
              (c :computed-in test-universe))
             (:metaclass computed-class))))
      (declare (type standard-class class))
      (flet ((computed-slot-p (slot-name)
               (typep (find-slot class slot-name) 'computed-effective-slot-definition)))
        (finalize-inheritance class)
        (is (not (computed-slot-p 'a)))
        #+sbcl(is (not (computed-slot-p 'b)))
        (is (computed-slot-p 'c))))
    (defclass computed-4 ()
      ((slot-a
        :initform (compute-as 0))
       (slot-b
        :initform (compute-as 1)))
      (:metaclass computed-class))
    (let ((class
           (defclass computed-5 ()
             ((a)
              (c :computed-in test-universe))
             (:metaclass computed-class))))
      (flet ((computed-slot-p (slot-name)
               (typep (find-slot class slot-name) 'computed-effective-slot-definition)))
        (finalize-inheritance class)
        (is (not (computed-slot-p 'a)))
        (is (computed-slot-p 'c))))))

(def generic a-of (instance))

(def generic b-of (instance))

(def test subclassing1 ()
  (defclass super ()
    ((a
      :accessor a-of
      :initform (compute-as 42))
     (b
      :accessor b-of
      :initform (compute-as (1+ (a-of -self-)))))
    (:metaclass computed-class))

  (defclass level0-1 (super)
    ((x))
    (:metaclass computed-class))

  (defclass level0-2 (super)
    ((y))
    (:metaclass computed-class))

  (defclass level1-1 (level0-1)
    ((z))
    (:metaclass computed-class))

  (defclass sub (level1-1 level0-2)
    ((b
      :accessor b-of
      :initform (compute-as 0))
     (a
      :accessor a-of
      :initform (compute-as 1)))
    (:metaclass computed-class))

  (let ((sub (make-instance 'sub)))
    (is (= (a-of sub) 1))
    (is (= (b-of sub) 0))))

;;;;;;
;;; Instance tests

(def class computed-test-class ()
  ((slot-a
    :accessor slot-a-of
    :initarg :slot-a
    :computed-in test-universe)
   (slot-b
    :accessor slot-b-of
    :initarg :slot-b
    :computed-in test-universe))
  (:metaclass computed-class))

(def test valid ()
  (let ((object (make-instance 'computed-test-class)))
    (is (computed-slot-valid-p object 'slot-a))
    ;; need to setf a computed-state first
    (setf (slot-a-of object) (compute-as 1))
    (is (not (computed-slot-valid-p object 'slot-a)))
    ;; no need to wrap it anymore, it's already a computed state
    (setf (slot-a-of object) 1)
    (is (computed-slot-valid-p object 'slot-a))
    (invalidate-computed-slot object 'slot-a)
    (is (not (computed-slot-valid-p object 'slot-a)))))

(def test boundp1 ()
  (let ((object (make-instance 'computed-test-class)))
    (signals unbound-slot (slot-a-of object))
    (setf (slot-a-of object) (compute-as 1))
    (setf (slot-b-of object) (compute-as (1+ (slot-a-of -self-))))
    (is (= 2 (slot-b-of object)))
    (setf (slot-a-of object) 2)
    (is (= 2 (slot-a-of object)))
    (is (= 3 (slot-b-of object)))
    (slot-makunbound object 'slot-a)
    (is (not (slot-boundp object 'slot-a)))
    (signals unbound-slot (slot-a-of object))))

(def test compute1 ()
  (let ((object (make-instance 'computed-test-class
                               :slot-a (compute-as 1)
                               :slot-b (compute-as (1+ (slot-a-of -self-))))))
    (is (= 1 (slot-a-of object)))
    (is (= 2 (slot-b-of object)))
    (setf (slot-a-of object) 2)
    (is (= 2 (slot-a-of object)))
    (is (= 3 (slot-b-of object)))))

(def test compute2 ()
  (let* ((object-1 (make-instance 'computed-test-class
                                  :slot-a (compute-as 1)
                                  :slot-b (compute-as (1+ (slot-a-of -self-)))))
         (object-2 (make-instance 'computed-test-class
                                  :slot-a (compute-as (+ (slot-a-of object-1) (slot-b-of object-1)))
                                  :slot-b (compute-as (1+ (slot-a-of -self-))))))
    (is (= 4 (slot-b-of object-2)))
    (setf (slot-a-of object-1) 2)
    (is (= 6 (slot-b-of object-2)))))

(def test compute3 ()
  (let* ((object-1 (make-instance 'computed-test-class
                                  :slot-b (compute-as (1+ (slot-a-of -self-)))))
         (object-2 (make-instance 'computed-test-class
                                  :slot-a (compute-as (+ (slot-a-of object-1) (slot-b-of object-1)))
                                  :slot-b (compute-as (1+ (slot-a-of -self-))))))
    (signals unbound-slot (slot-a-of object-1))
    (setf (slot-a-of object-1) (compute-as 0))
    (is (= 0 (slot-a-of object-1)))
    (is (= 2 (slot-b-of object-2)))
    (setf (slot-a-of object-1) 2)
    (setf (slot-b-of object-2) (compute-as (* 2 (slot-a-of -self-))))
    (is (= 2 (slot-a-of object-1)))
    (is (= 5 (slot-a-of object-2)))
    (is (= 10 (slot-b-of object-2)))))

(def test compute4 ()
  (setf (find-class 'sbcl-class-cache-computed-test) nil)
  (defclass sbcl-class-cache-computed-test ()
    ((slot-a :accessor slot-a-of :initarg :slot-a)
     (slot-b :accessor slot-b-of :initarg :slot-b))
    (:metaclass computed-class))
  (let ((object (make-instance 'sbcl-class-cache-computed-test :slot-a (compute-as 1) :slot-b 1)))
    (slot-a-of object)
    (slot-b-of object))
  (defclass sbcl-class-cache-computed-test ()
    ((slot-a :accessor slot-a-of :initarg :slot-a :computed-in test-universe)
     (slot-b :accessor slot-b-of :initarg :slot-b :computed-in test-universe))
    (:metaclass computed-class))
  (let ((object (make-instance 'sbcl-class-cache-computed-test
                               :slot-a (compute-as 1)
                               :slot-b (compute-as (1+ (slot-a-of -self-))))))
    (is (= 1 (slot-a-of object)))
    (is (= 2 (slot-b-of object)))
    (setf (slot-a-of object) 2)
    (is (= 3 (slot-b-of object)))))

;;;;;;
;;; always recomputed stuff

(def special-variable *always-compute-global-counter* 0)

(def test always-compute-with-object ()
  (setf *always-compute-global-counter* 0)
  (let* ((object (make-instance 'computed-test-class
                               :slot-a (compute-as* (:recomputation-mode :always)
                                         (slot-b-of -self-) ; read a slot to check later that we don't depend on it
                                         (incf *always-compute-global-counter*))
                               :slot-b (compute-as 42)))
         (slot-a-state (computed-state-or-nil object (find-slot 'computed-test-class 'slot-a))))
    (is (not (null slot-a-state)))
    (is (= (slot-a-of object) 1))
    (is (= *always-compute-global-counter* 1))

    (is (= (slot-a-of object) 2))
    (is (= *always-compute-global-counter* 2))

    (is (null (cs-depends-on slot-a-state)))))

(def test always-compute-with-clet ()
  (setf *always-compute-global-counter* 0)
  (clet ((a (compute-as 42))
         (b (compute-as* (:recomputation-mode :always)
              (format nil "~A" a)                       ; read a var to check later that we don't depend on it
              (incf *always-compute-global-counter*))))
    (is (= a 42))
    (is (= b 1))
    (is (= *always-compute-global-counter* 1))

    (is (= a 42))
    (is (= b 2))
    (is (= *always-compute-global-counter* 2))

    (is (null (cs-depends-on b-state)))))

;;;;;;
;;; Reconfiguration tests

(def test reconfigure1 ()
  (let* ((object (make-instance 'computed-test-class)))
    (setf (slot-a-of object) nil)
    (setf (slot-b-of object) nil)
    (setf (slot-a-of object) (compute-as 1))
    (setf (slot-b-of object) (compute-as (1+ (slot-a-of -self-))))
    (is (= 2 (slot-b-of object)))
    (setf (slot-a-of object) (compute-as (* 6 7)))
    (is (= 43 (slot-b-of object)))
    (setf (slot-a-of object) nil)
    (is (null (slot-a-of object)))
    (setf (slot-b-of object) (compute-as (not (slot-a-of -self-))))
    (is (not (null (slot-b-of object))))))

(def test reconfigure2 ()
  (let ((object (make-instance 'computed-test-class)))
    (flet ((current-pulse ()
             (awhen (computed-state-or-nil object (find-slot (class-of object) 'slot-b))
               (current-pulse it))))
      (setf (slot-a-of object) 1)
      (setf (slot-b-of object) (compute-as (1+ (slot-a-of object))))
      (is (= 1 (slot-a-of object)))
      (is (= 2 (slot-b-of object)))
      (make-slot-uncomputed object 'slot-a)
      (let ((pulse (current-pulse)))
        (is (= 1 (slot-a-of object)))
        (is (= 2 (slot-b-of object)))
        (setf (slot-a-of object) 42)
        (is (not (computed-state-p (standard-instance-access-form object (find-slot (class-of object) 'slot-a)))))
        (is (= 2 (slot-b-of object)))
        (is (= pulse (current-pulse)))
        (setf (slot-a-of object) (compute-as 42)) ; make it computed again
        (is (= (+ pulse 1) (current-pulse)))
        (is (= 2 (slot-b-of object))) ; dependency should not have been registered, so it should not get recalculated
        (invalidate-computed-slot object 'slot-b)
        (is (= 43 (slot-b-of object)))))))

;;;;;;
;;; clet tests

(def test clet1 ()
  (clet ((a (compute-as 1))
         (b (compute-as (1+ a)))
         (c (compute-as (+ a b))))
    (is (eq 'variable (cs-kind a-state)))
    (is (= c 3))
    (setf a 2)
    (is (= c 5))
    (signals error (setf a (compute-as 42))) ; this is not the way to do it
    (let ((old-a-state a-state))
      (setf a-state (compute-as 42))    ; this setf also invalidates the state of 'b' and 'c'
      (is (eq a-state old-a-state)))    ; should keep its identity
    (is (= c 85))
    (setf a 43)
    (is (= c 87))))

(def test clet2 ()
  (clet ((a 42)
         (b (compute-as (1+ a)))
         (c (compute-as (1+ b))))
    (locally (declare #+sbcl(sb-ext:muffle-conditions warning))
      (signals unbound-variable (print a-state)))
    (is (= a 42))
    (is (= b 43))
    (is (= c 44))
    (setf a 2)                          ; does not invalidate anything, that's a simple let* binding
    (is (= a 2))
    (is (= b 43))
    (is (= c 44))
    (invalidate-computed-state b-state)
    (is (= b 3))
    (is (= c 4))))

(def test clet3 ()
  ;; same as computed-class/clet/2, but with variable capturing
  (let (a-reader
        a-writer
        b-reader
        b-writer
        b-state-reader
        c-reader)
    (clet ((a 42)
           (b (compute-as (1+ a)))
           (c (compute-as (1+ b))))
      (setf a-reader (lambda () a)
            b-reader (lambda () b)
            c-reader (lambda () c)
            a-writer (lambda (value) (setf a value))
            b-writer (lambda (value) (setf b value))
            b-state-reader (lambda () b-state)))
    (is (= (funcall a-reader) 42))
    (is (= (funcall b-reader) 43))
    (is (= (funcall c-reader) 44))
    (funcall a-writer 2)                ; does not invalidate anything, that's a simple let* binding
    (is (= (funcall a-reader) 2))
    (is (= (funcall b-reader) 43))
    (is (= (funcall c-reader) 44))
    (invalidate-computed-state (funcall b-state-reader)) ; should also invalidate 'c'
    (is (= (funcall b-reader) 3))
    (is (= (funcall c-reader) 4))))

(def test clet4 ()
  (clet ((a (compute-as 2))
         (object (make-instance 'computed-test-class
                                :slot-a (compute-as (1+ a))
                                :slot-b (compute-as (1+ (slot-a-of -self-))))))
    (is (= a 2))
    (clet ((b (compute-as (+ a (slot-b-of object)))))
      (is (= b 6))
      (debug-only (is (eq (cs-place-descriptor b-state) 'b)))
      (is (= (slot-a-of object) 3))
      (is (= (slot-b-of object) 4))
      (setf (slot-a-of object) 42)
      (is (= a 2))
      (is (= b 45))
      (is (= (slot-a-of object) 42))
      (is (= (slot-b-of object) 43)))))

#|

dragons be here :)

(defcparameter foo (compute-as 50)) ;; we must define it on toplevel once, so the resulting symbol will be dynamic

(def test (clet-global1 :compile-before-run #f) ()
  (setf foo (compute-as 50))

  (clet ((a (compute-as (1+ foo)))
         (object (make-instance 'computed-test-class
                                :slot-a (compute-as (1+ a)))))
    (is (= foo 50))
    (is (= a 51))
    (is (= (slot-a-of object) 52))
    (debug-only (is (eq (cs-place-descriptor foo-state) 'foo)))
    (let ((previous-state foo-state))
      (setf foo-state (compute-as 1))
      (is (not (eq previous-state foo-state)))
      (is (= 2 a)))))
|#

(def test pulse1 ()
  (let* ((object (make-instance 'computed-test-class
                                :slot-a (compute-as 1)
                                :slot-b (compute-as (1+ (slot-a-of -self-))))))
    (flet ((current-pulse ()
             (awhen (computed-state-or-nil object (find-slot (class-of object) 'slot-a))
               (current-pulse it))))
      (let ((pulse (current-pulse)))
        (setf (slot-a-of object) 2)
        (is (= (current-pulse) (+ 1 pulse)))
        (slot-b-of object)
        (is (= (current-pulse) (+ 1 pulse)))))))

(def test circularity1 ()
  (let* ((circularity #f)
         (flag #f)
         (object (make-instance 'computed-test-class
                                :slot-a (compute-as
                                          (when (or circularity flag)
                                            (slot-b-of -self-)))
                                :slot-b (compute-as
                                          (when (or circularity (not flag))
                                            (slot-a-of -self-))))))
    (setf flag #f)
    (invalidate-computed-slot object 'slot-a)
    (invalidate-computed-slot object 'slot-b)
    (is (null (slot-a-of object)))
    (is (null (slot-b-of object)))

    (setf flag #t)
    (invalidate-computed-slot object 'slot-a)
    (invalidate-computed-slot object 'slot-b)
    (is (null (slot-a-of object)))
    (is (null (slot-b-of object)))

    (setf circularity #t)
    (invalidate-computed-slot object 'slot-a)
    (invalidate-computed-slot object 'slot-b)
    (signals error (slot-a-of object))
    (signals error (slot-b-of object))))


;;;;;;
;;; universe-separation tests

(def test universe-separation1 ()
  (clet ((a (compute-as 1))
         (b (separated-compute-as (1+ a)))
         (c (compute-as (+ a b))))
    (is (= a 1))
    (is (= c 3))
    (setf a 42)
    (is (= c 44))
    (is (= 1 (length (cs-depends-on c-state))))
    (is (= 0 (length (cs-depends-on b-state))))))


;;;;;;
;;; defcfun tests

;; define some lexical variables, some of them are computed
(clet ((words-to-be-uppercased (compute-as '()))
       (integers-to-be-factorized (compute-as '()))
       (irrelevant-variable (separated-compute-as nil))
       (run-counter)
       (memoize-table))

  (setf memoize-table
        (second (multiple-value-list
                    ;; define a function that is using the computed lexical bindings that we will be
                    ;; modifying in various ways in the test run and check the function's behaviour
                    (defcfun (format* :computed-in test-universe) (datum &rest args)
                      (incf run-counter)
                      irrelevant-variable ; read a computed-state that is in another universe: should have no effect at all.
                      (labels ((simple-factorial (n)
                                 (if (= n 1)
                                     1
                                     (* n (simple-factorial (- n 1))))))
                        (apply #'format nil datum
                               (loop for arg in args
                                     collect (cond ((and (stringp arg)
                                                         (member arg words-to-be-uppercased :test #'string=))
                                                    (string-upcase arg))
                                                   ((and (integerp arg)
                                                         (member arg integers-to-be-factorized :test #'eql))
                                                    (simple-factorial arg))
                                                   (t arg)))))))))

  ;; we need to tell Stefil to compile the test body at definition time, so it can see the lexical bindings
  (def test (defcfun1 :compile-before-run #f) ()

    (setf run-counter 0)
    (setf words-to-be-uppercased '())
    (setf integers-to-be-factorized '())

    (let ((test-datum "~A-~A-~A")
          (test-args (list "foo" "bar" 42)))

      (macrolet ((is* (expected)
                   `(is (string= ,expected (apply 'format* test-datum test-args)))))
        (is* "foo-bar-42")
        (is (= run-counter 1))
        (is* "foo-bar-42")
        (is (= run-counter 1))

        (setf irrelevant-variable 123)
        (is* "foo-bar-42")
        (is (= run-counter 1))

        (push 1 integers-to-be-factorized)

        (is* "foo-bar-42")
        (is (= run-counter 2))
        (is* "foo-bar-42")
        (is (= run-counter 2))

        (push "bar" words-to-be-uppercased)
        (is* "foo-BAR-42")
        (is (= run-counter 3))
        (is* "foo-BAR-42")
        (is (= run-counter 3))

        (push 42 integers-to-be-factorized)
        (is* "foo-BAR-1405006117752879898543142606244511569936384000000000")
        (is (= run-counter 4))
        (is* "foo-BAR-1405006117752879898543142606244511569936384000000000")
        (is (= run-counter 4))

        (dotimes (j 1000)
          (dotimes (i 100)
            ;; to test the speed without the memoize cache:
            ;;(invalidate-computed-state integers-to-be-factorized-state)
            (format* test-datum "foo" "bar" i)))

        (is (= run-counter 103))
        ;; to inspect the memoize-table:
        ;;(break "The memoized-table is ~A" memoize-table)
        (is (= (hash-table-count memoize-table) 100))))))

(defcfun (fun-with-multiple-values :computed-in test-universe) (some arg &key key)
  (values key arg some))

(def test defcfun2 ()
  (is (equal (list 3 2 1) (multiple-value-list (fun-with-multiple-values 1 2 :key 3)))))


;;;;;;
;;; Timing tests

(def class standard-test ()
  ((slot-a
    :accessor slot-a-of
    :initarg :slot-a
    :initform 0)
   (slot-b
    :accessor slot-b-of
    :initarg :slot-b
    :initform 0)))

(def test timing1 ()
  (flet ((measure (object message)
           (setf (slot-a-of object) 0)
           #+sbcl(sb-ext:gc :full t)
           (terpri *debug-io*)
           (write-line message *debug-io*)
           (terpri *debug-io*)
           (common-lisp:time
            (dotimes (counter 4000000)
              (slot-b-of object)))
           (terpri *debug-io*)))
    (measure (make-instance 'standard-test) "*** Reader, no computation, standard accessor: ")
    (measure (make-instance 'computed-test-class
                            :slot-a (compute-as 0)
                            :slot-b (compute-as (1+ (slot-a-of -self-))))
             "*** Reader, no computation, computed accessor: ")))

(def test timing2 ()
  (flet ((measure (object message)
           #+sbcl(sb-ext:gc :full t)
           (terpri *debug-io*)
           (write-line message *debug-io*)
           (terpri *debug-io*)
           (common-lisp:time
            (dotimes (counter 1000000)
              (setf (slot-a-of object) counter)
              (slot-b-of object)))
           (terpri *debug-io*)))
    (measure (make-instance 'standard-test) "*** Reader, writer, no computation, standard accessor: ")
    (measure (make-instance 'computed-test-class
                            :slot-a (compute-as 0)
                            :slot-b (compute-as (1+ (slot-a-of -self-))))
             "*** Reader, writer, recomputation, computed accessor: ")))

#|
on SBCL 1.0.47.24
TEST> (timing1)

*** Reader, no computation, standard accessor:

Evaluation took:
  0.055 seconds of real time
  0.050000 seconds of total run time (0.050000 user, 0.000000 system)
  90.91% CPU
  122,985,760 processor cycles
  0 bytes consed

*** Reader, no computation, computed accessor:

Evaluation took:
  1.555 seconds of real time
  1.550000 seconds of total run time (1.550000 user, 0.000000 system)
  99.68% CPU
  3,514,500,109 processor cycles
  0 bytes consed



TEST> (timing2)

*** Reader, writer, no computation, standard accessor:

Evaluation took:
  0.049 seconds of real time
  0.050000 seconds of total run time (0.050000 user, 0.000000 system)
  102.04% CPU
  109,468,976 processor cycles
  0 bytes consed

*** Reader, writer, recomputation, computed accessor:

Evaluation took:
  1.675 seconds of real time
  1.680000 seconds of total run time (1.670000 user, 0.010000 system)
  [ Run times consist of 0.070 seconds GC time, and 1.610 seconds non-GC time. ]
  100.30% CPU
  3,787,267,642 processor cycles
  63,995,664 bytes consed
|#