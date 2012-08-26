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

;;;;----------------------------------------
;;;; History

(define-history-browsers script test-script-p)

;;;;----------------------------------------
;;;; Definitions

(define-test-object-predicate test-script)

;;;;----------------------------------------
;;;; Instantiator

(defun make-test-script-instance (script-name &rest args)
  (apply #'make-instance script-name args))

(defmethod initialize-instance :after ((obj test-script) &rest args &key &allow-other-keys)
  (declare (ignore args))
  (setf (tests-specified obj)
	(mapcar #'(lambda (step) (make-script-step-instance step obj))
		(test-script-steps obj)))
  (setf (tests-remaining obj) (tests-specified obj)))

;;;;----------------------------------------
;;;; Script step instantiator

(defun make-script-step-instance (step scri)
  (cond ((test-script-p step) (make-test-script-instance step :runner scri))
	((test-suite-p step) (make-test-suite-instance step :runner scri))
	((test-case-p step)
	 (make-test-suite-instance (test-case-defining-suite step)
				   :runner scri :cases (list step)))
	((and (consp step)
	      (test-suite-p (car step))
	      (every #'test-case-p (cdr step)))
	 (make-test-suite-instance (car step) :runner scri :cases (cdr step)))
	(t (error "Invalid step specification ~S in ~S" step scri))))

;;;;----------------------------------------
;;;; Definition

(defun record-test-script-steps-dependencies (steps)
  (dolist (step steps)
    (etypecase step
      (cons (record-dependence (car step) 'test-suite))
      (symbol (record-dependence step)))))

(defun record-test-script-dependencies (name)
  (record-test-script-steps-dependencies
   (test-script-steps (get-test-object-prototype (find-class name)))))

(defmacro undefscript (&optional (name (coerce-to-class-name (last-script))))
  `(when (test-script-p ',name)
     (defclass ,name (deleted-test-script) ())
     (mop:finalize-inheritance (find-class ',name))
     (hook-run 'test-deletion-hook ',name)
     ',name))

(defmacro defscript (name &key setup teardown steps)
  `(progn
     ;; First define the test script to ensure it loses all class allocation
     ;; slots from previous definitions. This has proven to be a problem on ACL.
     (defclass ,name (deleted-test-script) ())
     (mop:finalize-inheritance (find-class ',name))
     (defclass ,name (test-script)
       ())
     (defmethod test-script-steps ((obj ,name))
       ',steps)
     (defmethod test-script-setup-body ((obj ,name))
       ',setup)
     (defmethod test-script-teardown-body ((obj ,name))
       ',teardown)
     (new-test-object-data ',name)
     (record-test-script-steps-dependencies ',steps)
     (hook-run 'test-definition-hook ',name)
     ',name))

;;; EOF
