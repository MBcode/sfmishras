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
;;;; Definitions

(define-test-object-predicate test-object)

(defun get-test-object-prototype (object)
  (let ((object-class (coerce-to-class object)))
    (unless (mop:class-finalized-p object-class)
      (mop:finalize-inheritance object-class))
    (mop:class-prototype object-class)))

;;;;----------------------------------------
;;;; State transitions

(defun check-transition (from-state to-state)
  (assert (ecase from-state
	    (:init (eq to-state :running))
	    (:running (member to-state '(:success :fail :error :timeout)))
	    ((:success :fail :error :timeout) nil))
	  ()
	  "Illegal state transition from ~S to ~S." from-state to-state))

(defun state-transition (test-object to-state)
  (check-transition (test-state test-object) to-state)
  (setf (test-state test-object) to-state))

(defun running-p (test-object)
  (eq (test-state test-object) :running))

(defun finished-p (test-object)
  (case (test-state test-object)
    ((:success :fail :error) t)))

(defun attempted-p (test-object)
  (not (eq (test-state test-object) :init)))

(defun success-p (test-object)
  (eq (test-state test-object) :success))

(defun fail-p (test-object)
  (case (test-state test-object)
    ((:fail :error) t)))

;;; EOF
