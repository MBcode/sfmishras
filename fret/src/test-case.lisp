;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: FRET; Base: 10 -*-

#|
Copyright (c) 2004,2007, Sunil Mishra
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

(define-history-browsers case test-case-p)

;;;;----------------------------------------
;;;; Test case definitions

(defconstant +test-case-functions+ '())

(defmethod initialize-instance :after ((obj test-case) &rest args &key &allow-other-keys)
  (declare (ignore args))
  (use-test obj))

(defun make-test-case-instance (test &optional si)
  (let ((test-type (etypecase test
		     (symbol test)
		     (class test)
		     (test-case (class-name test)))))
    (make-instance test-type :invoking-suite si :runner (test-runner si))))

(define-test-object-predicate test-case)
  
(defun add-suite-direct-case (suite case-name)
  (let ((present-cases (get-suite-direct-cases suite)))
    (if present-cases
	(loop for rest-cases on present-cases
	      until (eq (car rest-cases) case-name)
	      unless (cdr rest-cases)
	      do (setf (cdr rest-cases) (list case-name)))
      (setf (get-suite-direct-cases suite) (list case-name)))
    case-name))

(defun delete-suite-direct-case (suite case-name)
  (let* ((present-cases (get-suite-direct-cases suite))
	 (new-cases (delete case-name present-cases)))
    (unless (eq present-cases new-cases)
      (setf (get-suite-direct-cases suite) new-cases))
    case-name))

;;;;----------------------------------------
;;;; Deleting test cases

(defun remove-root-case-methods (cc)
  ;; Delete the methods classes defined on the root cases
  (dolist (gf +test-case-functions+)
    (let* ((gf-object (the generic-function (symbol-function gf)))
	   (method (find-method gf-object '() (list cc) nil)))
      (when method
	(remove-method gf-object method)))))

(defmacro undefcase (&optional (case-name (coerce-to-class-name (last-case))))
  "Undefine the test case named by CASE-NAME. All generated definitions
are also deleted. If CASE-NAME is not specified, the value (LAST-CASE) is
undefined. The test case undefined is returned as the value."
  `(cond ((test-case-p ',case-name)
	  (let ((test-suite
		 (ignore-errors
		   (test-case-defining-suite
		    (get-test-object-prototype (find-class ',case-name))))))
	    (defclass ,case-name (deleted-test-case) ())
	    (mop:finalize-inheritance (find-class ',case-name))
	    (when test-suite
	      (delete-suite-direct-case test-suite ',case-name))
	    (hook-run 'test-deletion-hook ',case-name)
	    ',case-name))
	 (t (warn "~A does not name a test." ',case-name))))

;;;;----------------------------------------
;;;; Test definition

(defun name-anonymous-test-case (suite-name)
  (intern (concatenate 'string 
	    (symbol-name suite-name) 
	    (symbol-name '#:-test-case-)
	    (princ-to-string (increment-case-sequence suite-name)))
	  (symbol-package suite-name)))

(defgeneric test-case-body (i)
  (:documentation "This generic function is for the body class allocated
slot added to all executable test cases.")
  (:method ((i test-case))
	   (error "No test body found for ~A." i)))

;;;(defcase ()
;;; (insert-item b 2)
;;; (insert-item b 3)
;;; (delete-item b 2)
;;; (ensure-equal (size b) 1)))
(defmacro defcase ((&optional (suite-name (coerce-to-class-name (last-suite))) case-name)
		   &body body)
  "A test case definition. The syntax is:

    (defcase ([suite-name] [case-name])
	[documentation]
      <body>)

The suite-name defaults to the last suite executed or defined. And if
not specified, the case-name is generated automatically. The body of 
the test is executed as an implicit progn, and is expected to contain
one or more ENSURE statements (though this is not a requirement)."
  (let ((documentation nil)
	(anonymous-p nil))
    (when (stringp (car body))
      (setq documentation (pop body)))
    (unless case-name
      (setq case-name (name-anonymous-test-case suite-name)
	    anonymous-p t))
    `(progn
       ;; First define the test case to ensure it loses all class allocation
       ;; slots from previous definitions. This has proven to be a problem on ACL.
       (defclass ,case-name (deleted-test-case) ())
       (mop:finalize-inheritance (find-class ',case-name))
       (defclass ,case-name (test-case)
	 ()
	 ,@(when documentation
	     `((:documentation ,documentation))))
       (defmethod test-case-body ((obj ,case-name))
	 ',body)
       (defmethod test-case-anonymous-p ((obj ,case-name))
	 ',anonymous-p)
       (defmethod test-case-defining-suite ((obj ,case-name))
	 ',suite-name)
       (new-test-object-data ',case-name)
       (add-suite-direct-case ',suite-name ',case-name)
       (hook-run 'test-definition-hook ',case-name)
       ',case-name)))

;;; EOF
