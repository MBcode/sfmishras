;;; -*- Mode: LISP; Syntax: ANSI-COMMON-LISP; Package: MCPAT; Base: 10 -*-

#|
Copyright (c) 2000-2006, Sunil Mishra
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

;; $Id: encoded-ruleset.lisp 97 2006-08-31 06:00:10Z smishra $

(in-package :mcpat)

;;; Encoder

(defun get-encoder (pattern)
  (cond ((consp pattern) (get-encoder (car pattern)))
	((symbolp pattern) pattern)
	(t (error "Invalid encoder access on ~S." pattern))))

;;; Class and methods

(defclass encoded-ruleset (abstract-ruleset)
  ((encoder-sorting :accessor ruleset-encoder-sorting :initform nil)))

(defmethod clear-ruleset progn ((ruleset encoded-ruleset))
  (setf (ruleset-encoder-sorting ruleset) nil))

(defmethod unsort-rule-from-ruleset ((ruleset encoded-ruleset) rule)
  (let* ((encoder (rule-sort-data rule))
	 (encoder-rules (or (assoc encoder (ruleset-encoder-sorting ruleset))
			    (error "Could not find encoder ~S in ~S." encoder ruleset))))
    (delete (rule-name rule) encoder-rules)))

(defmethod sort-rule-into-ruleset ((ruleset encoded-ruleset) rule)
  (let* ((encoder (get-encoder (rule-pattern rule)))
	 (encoder-rules (assoc encoder (ruleset-encoder-sorting ruleset)))
	 (old-rule-list (when encoder-rules
			  (member (rule-name rule) (cdr encoder-rules)))))
    (assert (not (pattern-variable-p encoder)) ()
      "Encoder ~S must not be a variable." encoder)
    (setf (rule-sort-data rule) encoder)
    (cond (old-rule-list (setf (car old-rule-list) (rule-name rule)))
	  (encoder-rules (nconc encoder-rules (list (rule-name rule))))
	  (t (push (list encoder (rule-name rule)) (ruleset-encoder-sorting *ruleset*))))))

(defmethod solve-using-ruleset ((ruleset encoded-ruleset) form)
  (let ((encoder (get-encoder form)))
    (solve-using-ruleset-rules form (cdr (assoc encoder (ruleset-encoder-sorting ruleset)))
			       ruleset)))

;;; EOF
