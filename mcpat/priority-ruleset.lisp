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

;; $Id: priority-ruleset.lisp 97 2006-08-31 06:00:10Z smishra $

(in-package :mcpat)

(defclass priority-ruleset (abstract-ruleset)
  ((priority :accessor ruleset-priority
	     :initform (make-array 0 :fill-pointer t :adjustable t)
	     :documentation "Sorted array of priorities. Cheaper than keeping a queue.")))

(defmethod clear-ruleset progn ((ruleset priority-ruleset))
  ;; We don't have to do this, but doing so ensures that any
  ;; rule in the vector will be properly GC'ed.
  (let ((pv (ruleset-priority ruleset)))
    (loop for i from 0 below (length pv)
	do (setf (aref pv i) nil))
    (setf (fill-pointer pv) 0)))

(defmethod unsort-rule-from-ruleset ((ruleset priority-ruleset) rule)
  (let* ((pv (ruleset-priority ruleset))
	 (rule-index (position (rule-name rule) pv)))
    (loop for i from rule-index below (1- (length pv))
	do (setf (aref pv i) (aref pv (1+ i))))
    (vector-pop pv)))

(defun calculate-rule-priority (pattern)
  (let ((score 0)
	(var-found-p nil))
    (labels ((%calculate (rest-pattern)
	       (cond ((consp rest-pattern)
		      (%calculate (car rest-pattern))
		      (%calculate (cdr rest-pattern)))
		     ((pattern-variable-p rest-pattern)
		      (unless var-found-p
			(setq var-found-p t))
		      (incf score))
		     (var-found-p (incf score 2))
		     (t (incf score 3)))))
      (%calculate pattern)
      score)))

(defmethod sort-rule-into-ruleset ((ruleset priority-ruleset) rule)
  ;; Naive implementation that does a linear search
  ;; Being clever doesn't buy anything since rulesets are
  ;; expected to be infrequently updated.
  (let* ((pv (ruleset-priority ruleset))
	 (rule-priority (calculate-rule-priority (rule-pattern rule))))
    (setf (rule-sort-data rule) rule-priority)
    (vector-push-extend (rule-name rule) pv)
    (when (> (length pv) 1)
      (loop for i from (- (length pv) 2) downto 0
	  for other-priority = (rule-sort-data (gethash (aref pv i) (ruleset-rules ruleset)))
	  while (< other-priority rule-priority)
	  do (setf (aref pv (1+ i)) (aref pv i))
	  finally (setf (aref pv (1+ i)) (rule-name rule))))))

(defmethod solve-using-ruleset ((ruleset priority-ruleset) form)
  (solve-using-ruleset-rules form (ruleset-priority ruleset) ruleset))

;;; EOF
