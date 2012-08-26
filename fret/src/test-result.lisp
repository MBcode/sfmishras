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
;;;; Result output

(defun %print-case-fail (ti stream)
  (cond ((slot-boundp ti 'exception)
	 (format stream "DID-NOT-RUN because ~A" (test-case-exception ti))
	 (when (slot-boundp ti 'backtrace)
	   (fine "~&Backtrace:~%~A" (test-case-backtrace ti)))
	 'did-not-run)
	(t (princ 'failed stream)
	   (fine " because ~A" (test-case-exception ti))
	   'failed)))

(defun print-case-result (&optional (ti (last-case)))
  "Show the result of a test case. The current value of *test-output* is
used as the result destination. ti defaults to the last test case
run."
  (let ((*print-pretty* nil)
	(stream *test-output*))
    (format stream "~&~A: " ti)
    (cond ((not (attempted-p ti))
	   (princ 'not-attempted stream)
	   'not-attempted)
	  ((success-p ti)
	   (princ 'success stream)
	   'success)
	  (t (%print-case-fail ti stream)))))

(defun print-case-fail (&optional (ti (last-case)))
  (let ((*print-pretty* nil)
	(stream *test-output*)
	(*test-verbosity* :fine))
    (when (fail-p ti)
      (format stream "~&~A: " ti)
      (%print-case-fail ti stream))))

(defgeneric gather-test-case-summary (to)
  )

(defmethod gather-test-case-summary ((ti test-case))
  (let ((state (test-state ti)))
    (ecase state
      (:init (values 1 0 0 0))
      (:running (error "Does it make sense to get a summary while something is running?"))
      (:success (values 1 1 1 0))
      (:fail (values 1 1 0 1))
      ((:error :timeout) (values 1 1 0 0)))))

(defmethod gather-test-case-summary ((ri test-runner))
  (let ((specified 0)			; how many cases were specified?
	(attempted 0)			; how many test cases were attempted at all?
	(succeeded 0)			; of those attempted, how many succeeded?
	(failed 0))			; of those that didn't succeed, how many were failures?
					; The rest would be errors.
    (dolist (to (tests-specified ri))
      (multiple-value-bind (s-specified s-attempted s-succeeded s-failed)
	  (gather-test-case-summary to)
	(incf specified s-specified)
	(incf attempted s-attempted)
	(incf succeeded s-succeeded)
	(incf failed s-failed)))
    (values specified attempted succeeded failed)))
  
(defun print-result (&optional (ri (last-runner)))
  "Show the result from the execution of a test suite. ri defaults to the
last test runner."
  (let ((stream *test-output*))
    (multiple-value-bind (specified attempted succeeded failed)
	(gather-test-case-summary ri)
      (format stream "~2%Test execution summary for ~A:" ri)
      (format stream "~&  Total tests available:    ~D" specified)
      (format stream "~&  Tests attempted:          ~D" attempted)
      (format stream "~&      Tests that succeeded: ~D" succeeded)
      (format stream "~&      Tests that failed:    ~D" failed)
      (format stream "~&      Tests that erred:     ~D" (- attempted succeeded failed)))
    (values)))

(defmethod run-test :after ((ci test-case))
  (cond ((check-verbosity-threshold :normal) (print-case-result ci))
	((check-verbosity-threshold :severe) (print-case-fail ci))))

(defmethod run-test :after ((ri test-runner))
  (when (or (check-verbosity-threshold :fine)
	    (and (check-verbosity-threshold :severe)
		 (eq ri (test-runner ri))))
    (print-result ri)))

;;; EOF
