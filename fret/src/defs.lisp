;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: FRET; Base: 10 -*-

#|
Copyright (c) 2004, Sunil Mishra
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.
    * The names of its contributors may not be used to endorse or promote
      products derived from this software without specific prior written
      permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.
|#

;;; $Id$

(in-package #:fret)

;;; The root class definitions and the minimal support code for them.

;;;;----------------------------------------
;;;; General variables

(defvar *test-break-on-errors* t
  "If set, a test that fails breaks in a stack debugger.")

;;;;----------------------------------------
;;;; Class hierarchy

(defclass test-object ()
  ((start-time :initform (get-universal-time) :reader test-start-time)
   (state :initform :init :accessor test-state)
   (runner :initarg :runner :initform (error-required 'runner 'test-object) :reader test-runner)))

(defclass test-result (test-output-mixin)
  ((specified :accessor tests-specified)
   (remaining :accessor tests-remaining)))

(defclass test-runner (test-object test-result test-output-mixin)
  ((break-on-errors :initarg :break-on-errors :initform *test-break-on-errors*
		    :accessor test-runner-break-on-errors))
  (:default-initargs :runner nil))

(defclass test-suite (test-runner)
  ())

(defclass test-script (test-runner)
  ())

(defclass test-case (test-object)
  ((invoking-suite :initarg :invoking-suite :initform (error-required 'invoking-suite 'test-case)
		   :reader test-case-invoking-suite)
   (exception :accessor test-case-exception
	      :documentation "Set only if the test fails.")
   (backtrace :accessor test-case-backtrace
	      :documentation "Set only if the test throws an error.")))

(defclass deleted-test-object () ())

(defclass deleted-test-script (deleted-test-object) ())

(defclass deleted-test-suite (deleted-test-object) ())

(defclass deleted-test-case (deleted-test-object) ())

;;;;----------------------------------------
;;;; CLOS Customizations

(defmethod update-instance-for-redefined-class :around
    ((instance test-object) added-slots discarded-slots property-list &rest initargs)
  ;; This method handles the redefinition of test objects. Test objects
  ;; should error if at initialization time there isn't a runner specified.
  ;; Except this should not happen at update-for-redefinition time, especially
  ;; if the redefinition is for the class prototype instance. SBCL implements
  ;; class prototypes as having all the slot values re-initialized based on
  ;; the initargs, even though prototypes aren't constructed with their slots
  ;; thus initialized. This can cause errors if the default initarg for the
  ;; slot is a function that signals a "not supplied" error. To catch such an
  ;; interpretation of the ANSI standard, we have this method.
  (declare (ignore added-slots discarded-slots property-list initargs))
  (handler-bind ((slot-required (lambda (c)
				  (invoke-restart (find-restart 'continue c)))))
    (call-next-method)))

;;;;----------------------------------------
;;;; Generic Operations

;;; The implementations will be present in individual files.

(defgeneric test-case-defining-suite (test-case)
  (:documentation "Returns the test suite name for this test case. Methods
of this generic function are dynamically generated.")
  (:method ((case-name symbol))
    (test-case-defining-suite (get-test-object-prototype (find-class case-name)))))

(defgeneric get-suite-slot-names (suite)
  (:method ((suite test-suite))
	   nil))

(defgeneric get-suite-slot-definitions (si)
  )

(defgeneric get-suite-class-options (si)
  )

(defgeneric delete-test-from-storage (test storage)
  (:method (test storage)
    (declare (ignore test storage))
    nil))

(defgeneric test-script-setup-body (scri)
  )

(defgeneric test-script-teardown-body (scri)
  )

(defgeneric run-setup (o)
  (:method :before ((o test-runner))
	   (running2 "Setup" o))
  (:method ((ri test-runner))
	   t)
  (:method ((scri test-script))
	   (eval `(progn ,@(test-script-setup-body scri)))))

(defgeneric run-teardown (o)
  (:method :before ((o test-runner))
	   (running2 "Teardown" o))
  (:method ((o test-runner))
	   t)
  (:method ((scri test-script))
	   (eval `(progn ,@(test-script-teardown-body scri)))))

(defgeneric test-script-steps (o))

(defgeneric (setf test-script-steps) (val o))

;;;;----------------------------------------
;;;; Utility Macros

(defmacro define-test-object-predicate (class)
  (let ((pred-name (intern (concatenate 'string (symbol-name class) (symbol-name '#:-p)) :fret)))
    `(defun ,pred-name (obj)
       (cond ((symbolp obj)
	      (subtypep (or (find-class obj nil) (return-from ,pred-name nil))
			',class))
	     ((typep obj 'standard-class)
	      (subtypep obj ',class))
	     (t (typep obj ',class))))))

;;; (defun last-suite ()
;;;   "Get the last test suite run or defined."
;;;   (find-if #'test-suite-p *test-stack*))
;;; 
;;; (defun next-suite ()
;;;   (next-matching-test #'test-suite-p))
;;; 
;;; (defun prev-suite ()
;;;   (previous-matching-test #'test-suite-p))
(defmacro define-history-browsers (suffix predicate)
  (let ((last-sym (intern (concatenate 'string (symbol-name '#:last-) (symbol-name suffix))))
	(next-sym (intern (concatenate 'string (symbol-name '#:next-) (symbol-name suffix))))
	(prev-sym (intern (concatenate 'string (symbol-name '#:prev-) (symbol-name suffix)))))
    `(progn
       (defun ,last-sym () (find-if #',predicate *test-stack*))
       (defun ,next-sym () (next-matching-test #',predicate))
       (defun ,prev-sym () (previous-matching-test #',predicate))
       (eval-when (:load-toplevel :execute)
	 (export (list ',last-sym ',next-sym ',prev-sym))))))

;;;;----------------------------------------
;;;; Test object data

(defvar *test-object-data* (make-hash-table))

(defstruct (test-object-data (:conc-name test-object-))
  )

(defun get-test-object-data (object)
  (gethash (coerce-to-class-name object) *test-object-data*))

(defun new-test-object-data (object-name)
  (assert (symbolp object-name))
  (setf (gethash object-name *test-object-data*) 
    (make-test-object-data)))

(defun remove-test-object-data (object)
  (remhash (coerce-to-class-name object) *test-object-data*))

(defun get-test-object-property (object property)
  (declare (ignore object property))
  (error "No properties."))
;;  (ecase property
;;    (storage (test-object-storage (get-test-object-data object)))
;;    ))

(defun set-test-object-property (object property value)
  (declare (ignore object property value))
  (error "No properties."))
;;  (ecase property
;;    (storage (setf (test-object-storage (get-test-object-data object)) value))
;;    ))

(defsetf get-test-object-property set-test-object-property)

;;;;----------------------------------------
;;;; Test suite data

(defstruct (test-suite-data (:conc-name test-suite-)
			    (:include test-object-data))
  sequence
  setup
  teardown
  direct-cases
  )

(defun get-test-suite-data (suite)
  (get-test-object-data suite))

(defun new-test-suite-data (suite-name sequence)
  (assert (symbolp suite-name))
  (setf (gethash suite-name *test-object-data*) 
    (make-test-suite-data :sequence sequence)))

(defun remove-test-suite-data (suite)
  (remove-test-object-data suite))

(defun get-test-suite-property (suite property)
  (case property
    (sequence (test-suite-sequence (get-test-object-data suite)))
    (setup (test-suite-setup (get-test-object-data suite)))
    (teardown (test-suite-teardown (get-test-object-data suite)))
    (direct-cases (test-suite-direct-cases (get-test-object-data suite)))
    (t (get-test-object-property suite property))
    ))

(defun set-test-suite-property (suite property value)
  (case property
    (sequence (setf (test-suite-sequence (get-test-object-data suite)) value))
    (setup (setf (test-suite-setup (get-test-object-data suite)) value))
    (teardown (setf (test-suite-teardown (get-test-object-data suite)) value))
    (direct-cases (setf (test-suite-direct-cases (get-test-object-data suite)) value))
    (t (set-test-object-property suite property value))
    ))

(defsetf get-test-suite-property set-test-suite-property)

;;; Then the suite specific properties

(defun increment-case-sequence (suite)
  (incf (get-test-suite-property suite 'sequence)))

(defun get-case-sequence (suite)
  (get-test-suite-property suite 'sequence))

(defun get-suite-setup (suite)
  (get-test-suite-property suite 'setup))

(defun set-suite-setup (suite value)
  (setf (get-test-suite-property suite 'setup) value))

(defsetf get-suite-setup set-suite-setup)

(defun get-suite-teardown (suite)
  (get-test-suite-property suite 'teardown))

(defun set-suite-teardown (suite value)
  (setf (get-test-suite-property suite 'teardown) value))

(defsetf get-suite-teardown set-suite-teardown)

(defun get-suite-fixture (suite fixture)
  (ecase fixture
    ((setup :setup) (get-suite-setup suite))
    ((teardown :teardown) (get-suite-teardown suite))))

(defun set-suite-fixture (suite fixture value)
  (ecase fixture
    ((setup :setup) (setf (get-suite-setup suite) value))
    ((teardown :teardown) (setf (get-suite-teardown suite) value))))

(defsetf get-suite-fixture set-suite-fixture)

(defun get-suite-direct-cases (suite)
  (get-test-suite-property suite 'direct-cases))

(defun set-suite-direct-cases (suite value)
  (setf (get-test-suite-property suite 'direct-cases) value))

(defsetf get-suite-direct-cases set-suite-direct-cases)

;;;;----------------------------------------
;;;; Hook Definitions

;;; The hooks are asymmetric because test suites create a different
;;; type of data store than other test objects.

(define-hook test-definition-hook (test)
  (history #'use-test))

(define-hook test-deletion-hook (test)
  (history #'unuse-test))

;;; EOF
