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

;;; $Id: test-runner.lisp 52 2004-08-29 06:28:55Z smishra $

(in-package #:fret)

;;;; This class is special. Its definition is not in defs. The object
;;;; and associated methods and functions manipulate global parameters
;;;; that must be defined in the same unit for compilation to happen
;;;; without warnings.

;;;;----------------------------------------
;;;; Definition

(defvar +verbosity-levels+ '(nil :none nil :severe t :normal t :all :fine :all)
  "t is the same as :normal, nil as :none. :fine is the most detailed,
so we equate it to :all. Repeating as we have allows a simple handling
of these equivalences using member.")

(defvar *test-verbosity* t
  "The frequency and quantity of feedback from test cases. Setting this
variable's value is equivalent to giving a value to the :verbosity argument
in run-suite and run-case. Valid vlaues are nil, :none (equivalent to nil),
:severe, t, :normal (equivalent to t), :all, and :fine (equivalent to :all).")

(defvar *test-print-length* 10
  "Equivalent of *print-length* for testing.")

(defvar *test-print-level* 5
  "Equivalent of *print-level* for testing.")

(defvar *test-output* *standard-output*
  "The stream to which test output should be sent. Setting this variable is
equivalent to specifying the stream argument in run-suite, run-case,
print-suite-result and print-case-result.")

(defclass test-output-mixin ()
  ((verbosity :initarg :verbosity :initform *test-verbosity* :accessor test-verbosity)
   (output :initarg :output :initform *test-output* :accessor test-output)
   (print-length :initarg :print-length :initform *test-print-length* :accessor test-print-length)
   (print-level :initarg :print-level :initform *test-print-level* :accessor test-print-level)))

(defmethod initialize-instance :after ((obj test-output-mixin) &rest args
				       &key runner
				            (verbosity nil verbosity-supplied-p)
				            (output nil output-supplied-p)
				            (print-length nil print-length-supplied-p)
				            (print-level nil print-level-supplied-p)
				       &allow-other-keys)
  (declare (ignore args verbosity output print-level print-length))
  (when runner
    (unless verbosity-supplied-p
      (setf (test-verbosity obj) (test-verbosity runner)))
    (unless output-supplied-p
      (setf (test-output obj) (test-output runner)))
    (unless print-length-supplied-p
      (setf (test-print-length obj) (test-print-length runner)))
    (unless print-level-supplied-p
      (setf (test-print-level obj) (test-print-level runner)))))

;;;;----------------------------------------
;;;; Dribble functions

(defmacro with-output-parameters ((output-mixin &key output verbosity print-level print-length)
				  &body body)
  `(let ((*test-output* (or ,output (test-output ,output-mixin)))
	 (*test-verbosity* (or ,verbosity (test-verbosity ,output-mixin)))
	 (*test-print-level* (or ,print-level (test-print-level ,output-mixin)))
	 (*test-print-length* (or ,print-length (test-print-length ,output-mixin))))
     ,@body))

(defun check-verbosity-threshold (level)
  (member *test-verbosity*
	  (or (member level +verbosity-levels+)
	      (error "Invalid verbosity level ~A." level))))

(defun out (level str args)
  ;; This is an internal function. We do not bother with nice &rest
  ;; in arglist.
  (when (check-verbosity-threshold level)
    (let ((*print-level* *test-print-level*)
	  (*print-length* *test-print-length*))
      (fresh-line *test-output*)
      (apply #'format *test-output* str args))))

(defun fine (str &rest args)
  (out :fine str args))

(defun normal (str &rest args)
  (out :normal str args))

(defun severe (str &rest args)
  (out :severe str args))

(defun running (task object)
  (out :normal "~A on ~A running..." (list task object)))

(defun running2 (task object)
  (out :fine "~A on ~A running..." (list task object)))

(defun message (str &rest args)
  "Output a user message as part of the test output."
  (out :normal "User output: ~?" (list str args)))

;;; EOF
