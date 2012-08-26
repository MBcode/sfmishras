;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: FRET; Base: 10 -*-

#|
Copyright (c) 2004,2006-2009, Sunil Mishra
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

;;;;----------------------------------------
;;;; History

(define-history-browsers suite test-suite-p)

;;;;----------------------------------------
;;;; Test suite definitions

(defvar +test-suite-option-keywords+ '(:setup :test :tests :teardown :case-sequence-start))

(defvar +test-suite-functions+
    '(get-suite-slot-names get-suite-slot-definitions get-suite-class-options
			   run-setup run-teardown))

(defmethod initialize-instance :after ((si test-suite) &rest args &key cases &allow-other-keys)
  (declare (ignore args))
  (setf (tests-specified si)
	(loop for case in (or cases (get-suite-cases si))
	      collect (make-test-case-instance case si)))
  (setf (tests-remaining si) (tests-specified si))
  (use-test si))

(defun make-test-suite-instance (suite-class &rest args)
  (apply #'make-instance suite-class args))

(define-test-object-predicate test-suite)

;;;;----------------------------------------
;;;; Deleting test suites and cases

(defun delete-suite-test-cases (suite-name)
  "Go through and delete the test cases defined to be part of the
current test suite."
  (dolist (test-class-signifier (ignore-errors (get-suite-cases suite-name)))
    (let ((test-class (etypecase test-class-signifier
			(symbol (find-class test-class-signifier))
			(class test-class-signifier))))
      (eval `(undefcase ,(class-name test-class))))))

(defun remove-suite-methods (sc)
  ;; Delete the methods defined on the suites
  (dolist (gf +test-suite-functions+)
    (let* ((gf-name (if (consp gf) (car gf) gf))
	   (eql-specialization-p (and (consp gf) (eql (cadr gf) 'eql)))
	   (gf-object (the generic-function (symbol-function gf-name)))
	   (spec-args (if eql-specialization-p `((eql ,sc)) (list sc)))
	   (method (find-method gf-object '() spec-args nil)))
      (when method
	(remove-method gf-object method)))))

(defmacro undefsuite (&optional (suite-name (coerce-to-class-name (last-suite))))
  "Delete the test suite suite-name, and remove all the method definitions
and test cases associated with the suite. If deleting the test suite also
deletes any sub-suites, then those too are undefined."
  ;; This is a hard one.
  ;; Neither CLOS nor MOP have a procedure for deleting a class.
  ;; So we redefine the class to be rooted in a non-suite class.
  ;; Any subclass of the suite that inherits from another suite
  ;; will remain a suite.
  `(progn
     (when (test-suite-p ',suite-name)
       (defclass ,suite-name (deleted-test-suite) ())
       (mop:finalize-inheritance (find-class ',suite-name))
       (dolist (subsuite-class (mop:class-direct-subclasses (find-class ',suite-name)))
	 (mop:finalize-inheritance subsuite-class)
	 (when (test-suite-p subsuite-class)
	   (remove-suite-supertype subsuite-class ',suite-name)))
       (dolist (subsuite-class (cdr (find-disconnected-class-subtree
				     (find-class ',suite-name) (find-class 'test-suite))))
	 (let ((subsuite-name (class-name subsuite-class)))
	   (eval `(defclass ,subsuite-name (deleted-test-suite) ())))
	 (remove-suite-methods subsuite-class))
       (dolist (case-name (get-suite-direct-cases ',suite-name))
	 (eval `(undefcase ,case-name)))
       (hook-run 'test-deletion-hook ',suite-name)
       ',suite-name)))

;;;;----------------------------------------
;;;; Test suite definition

(defun make-suite-class-definition (suite-name supers slot-defs class-options seq-start)
  `((delete-suite-test-cases ',suite-name)
    (defclass ,suite-name ,(or supers '(test-suite))
      ,slot-defs
      ,@class-options)
    (dolist (super ',supers)
      (record-dependence super 'test-suite))
    (defmethod get-suite-class-options ((si ,suite-name)) ',class-options)
    (new-test-suite-data ',suite-name ,seq-start)))

(defun collect-test-definitions (tests-keyword test-keywords)
  (append (cdr tests-keyword) (mapcar #'cdr test-keywords)))

(defun make-test-definition (test-defn test-suite)
  `(defcase ,(cons test-suite (car test-defn))
       ,@(cdr test-defn)))

(defun make-test-definitions (test-definitions test-suite)
  (loop for test-defn in test-definitions
      collect (make-test-definition test-defn test-suite)))

(defun fixture-executor-method-name (fkey)
  (intern (concatenate 'string (symbol-name '#:run-) (symbol-name fkey)) :fret))

(defun make-fixture-executor (fkey class body-forms)
  (let ((fmethod (fixture-executor-method-name fkey)))
    `(defmethod ,fmethod ((si ,class))
       ,@(when (eq fkey :setup) `((call-next-method)))
       (let* ((fn (make-suite-accessing-function si ',body-forms)))
	 (when fn
	   (funcall fn si)))
       ,@(when (eq fkey :teardown) `((call-next-method))))))

(defun process-fixture-forms (fkey suite-options suite-name)
  (let ((fixture (assoc fkey suite-options)))
    `(,(make-fixture-executor fkey suite-name (cdr fixture))
      (setf (get-suite-fixture ',suite-name ',fkey) ',(cdr fixture)))))

(defun make-suite-slot-accessors (suite-name slot-defs)
  `((defmethod get-suite-slot-definitions ((si ,suite-name))
      ',slot-defs)
    (defmethod get-suite-slot-names ((si ,suite-name))
      ,(if slot-defs
	   `(nconc (list ,@(loop for sdef in slot-defs
			       collect (list 'quote (car sdef))))
		   (call-next-method))
	 `(call-next-method)))))

;;;(defsuite test-binary-search-tree (test-containers)
;;;  ((b (make-container 'binary-search-tree)))
;;;  (:setup (empty! b))
;;;  (:tests
;;;   (() (ensure (empty-p b)))
;;;   (()
;;;    (insert-item b 2)
;;;    (ensure (not (empty-p b))))))
(defmacro defsuite (suite-name (&rest supers) (&rest slot-defs) &rest options)
  "Define a new test suite, and construct all associated definitions. The
arguments defsuite takes are a superset of the arguments to a DEFCLASS:

    (defsuite <suite-name> (<supers>)
      (<slot-definitions>)
      <suite-options>)

<supers> is a list of superclasses of the suite, which should only be
test suites themselves. If <supers> is NIL, we implicitly inherit from
TEST-SUITE. Inheriting from a CLOS class is not tested, and isn't testable
at definition time without assuming every superclass has been defined. We
do not test this circumstance, but such inheritence might cause improper
behavior.

The DEFSUITE specific options are all in the <suite-options> list. The
DEFSUITE specific options are:

:setup -    A list of forms that are evaluated as part of the suite setup.
            These forms are relevant to this suite and any subclass of this
            suite. Before running a test case in a suite, the setup forms 
            are executed from most general to most specific, and their 
            execution is repeated for each test case.

:teardown - A list of forms that are evaluated as part of the suite 
            teardown. These forms are relevant to this suite and any 
            subclass of this suite. After running a test case in a suite,
            the setup forms are executed from most specific to most 
            general, and their execution is repeated for each test case.

:test -     A test definition. The content of this option follows the form
            of a DEFTEST, except that the DEFCASE should not be included. 
            This option may be repeated.

:tests -    A list of test definitions as specified for :TEST above."
  (let ((suite-options nil)
	(class-options nil))
    (loop for opt in options
	  if (member (car opt) +test-suite-option-keywords+)
	  do (push opt suite-options)
	  else do (push opt class-options))
    (setq suite-options (nreverse suite-options)
	  class-options (nreverse class-options))
    (let* ((sequence-option (assoc :case-sequence-start suite-options))
	   (start-seq (or (cadr sequence-option) 0))
	   (tests-keyword (assoc :tests suite-options))
	   (test-keywords (remove :test suite-options :key #'car :test (complement #'eq)))
	   (all-test-cases (collect-test-definitions tests-keyword test-keywords))
	   (anonymous-case-count (count nil all-test-cases :key #'car)))
      (new-test-suite-data suite-name start-seq)
      `(progn
	 ,@(make-suite-class-definition suite-name supers slot-defs class-options
					(+ start-seq anonymous-case-count))
	 ,@(make-test-definitions all-test-cases suite-name)
	 ,@(make-suite-slot-accessors suite-name slot-defs)
	 ,@(process-fixture-forms :setup suite-options suite-name)
	 ,@(process-fixture-forms :teardown suite-options suite-name)
	 (hook-run 'test-definition-hook ',suite-name)
	 ',suite-name))))

;;;;----------------------------------------
;;;; Accessors

(defun get-suite-cases (suite)
  (let* ((suite-class (etypecase suite
			(symbol (find-class suite))
			(standard-class suite)
			(test-suite (class-of suite))))
	 (precedence-list (get-class-precedence-list suite-class))
	 (root-class (find-class 'test-suite)))
    (loop for suite-superclass in precedence-list
	  until (eq suite-superclass root-class)
	  append (get-suite-direct-cases suite-superclass))))

(defun make-suite-accessing-function (si body)
  (when body
    (eval `(lambda (,+suite-var+)
	     (with-slots (,@(get-suite-slot-names si)) ,+suite-var+
	       ,@body)))))

;;; EOF
