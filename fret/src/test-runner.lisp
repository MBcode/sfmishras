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

;;; NOTE
;;; Running a runner is different from running a case. Both of them
;;; are run. But a runner runs its constituents and its fixtures,
;;; while a case runs its body. It still makes sense for a single
;;; generic function to run both types of objects.

;;;;----------------------------------------
;;;; History

(define-history-browsers runner test-runner-p)

;;;;----------------------------------------
;;;; Definitions

(defmethod initialize-instance :after ((obj test-runner) &rest args &key runner &allow-other-keys)
  (declare (ignore args))
  (setf (slot-value obj 'runner) (or runner obj))
  (when runner
    (setf (test-runner-break-on-errors obj) (test-runner-break-on-errors runner))))

(define-test-object-predicate test-runner)

;;;;----------------------------------------
;;;; Backtrace

#+cmu
(defun get-stack-backtrace ()
  (with-output-to-string (str)
    (debug:backtrace most-positive-fixnum str)))

#+allegro
(defun get-stack-backtrace ()
  (with-output-to-string (str)
    (let ((*terminal-io* str)
	  (tpl:*zoom-display* 50)
	  (tpl:*zoom-print-level* *test-print-length*)
	  (tpl:*zoom-print-length* *test-print-level*)
	  (tpl:*zoom-print-circle* t)
	  (tpl:*zoom-print-special-bindings* t))
      (tpl:do-command "zoom" :from-read-eval-print-loop nil))))

#+sbcl
(defun get-stack-backtrace ()
  (with-output-to-string (str)
    (sb-debug:backtrace most-positive-fixnum str)))

#+ccl
(defun get-stack-backtrace ()
  (with-output-to-string (str)
    (pprint (ccl::backtrace-as-list) str)))

;;;;----------------------------------------
;;;; Case support

(defun test-case-fail (c ti)
  (let ((failed-ensure-p (typep c 'failed-ensure)))
    (severe (if failed-ensure-p "Test Case Failure: ~A" "Test Case Error: ~A")
	    c)
    (state-transition ti (if failed-ensure-p :fail :error))
    (setf (test-case-exception ti) c)
    (unless failed-ensure-p
      (setf (test-case-backtrace ti) (get-stack-backtrace))))
  nil)

;;;;----------------------------------------
;;;; run-test

(defgeneric run-test (tobj))

(defmethod run-test :around ((to test-output-mixin))
  (with-output-parameters (to)
    (call-next-method)))

(defmethod run-test :around ((ri test-runner))
  (let ((*test-break-on-errors* (test-runner-break-on-errors ri)))
    (call-next-method)))

(defmethod run-test :around ((scri test-script))
 (run-setup scri)
 (unwind-protect (call-next-method)
   (run-teardown scri)))

(defmethod run-test ((resi test-result))
  (loop (run-test (or (pop (tests-remaining resi))
		      (return)))))

(defmethod run-test :around ((ti test-case))
  (let ((*current-test* ti))
    (if *test-break-on-errors*
	(call-next-method)
      (handler-bind ((error (lambda (c) (invoke-restart (find-restart 'skip c)))))
	(call-next-method)))))

(defmethod run-test ((ti test-case))
  (running "Execute test" ti)
  (restart-case
      (loop
	(with-simple-restart (retry "Retry this test.")
          (state-transition ti :running)
	  (handler-bind ((error #'(lambda (c) (test-case-fail c ti))))
	    (let* ((si (test-case-invoking-suite ti))
		   (fn (make-suite-accessing-function si (test-case-body ti))))
	      ;; An error in the setup must be handled in the setup.
	      (run-setup si)
	      (running2 "Body" ti)
	      (unwind-protect
		  (funcall fn si)
		(run-teardown si))
	      (state-transition ti :success))))
	(return))
    (skip ()
	:report "Skip this test."
      nil)
    (disable ()
	:report "Disable this test and continue."
      (error "Not implemented.")))
  nil)

(defgeneric prepare-test-runner (tobj args))

(defmethod prepare-test-runner ((tobj test-runner) args)
  (destructuring-bind ((break-on-errors nil boe-supplied-p) (verbosity nil verbosity-supplied-p))
      args
    (when boe-supplied-p
      (setf (test-runner-break-on-errors tobj) break-on-errors))
    (when verbosity-supplied-p
      (setf (test-verbosity tobj) verbosity)))
  tobj)

(defmethod prepare-test-runner ((tcls symbol) args)
  (cond ((test-runner-p tcls) (apply #'make-instance tcls args))
	((test-case-p tcls)
	 (apply #'make-instance (test-case-defining-suite tcls) :case tcls args))
	(t (error "Invalid input ~S." tcls))))

(defun run (&rest args)
  (let ((runner
	 (if (test-object-p (car args))
	     (prepare-test-runner (car args) (cdr args))
	   (prepare-test-runner (last-test) args))))    
    (run-test runner)))

;;; EOF
