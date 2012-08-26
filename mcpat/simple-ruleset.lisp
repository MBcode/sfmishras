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

;; $Id: simple-ruleset.lisp 97 2006-08-31 06:00:10Z smishra $

(in-package :mcpat)

(defclass ruleset (abstract-ruleset)
  ((rule-list :accessor ruleset-rule-list :initform nil)))

(defmethod clear-ruleset progn ((ruleset ruleset))
  (setf (ruleset-rule-list ruleset) nil))

(defmethod unsort-rule-from-ruleset ((ruleset ruleset) rule)
  (setf (ruleset-rule-list ruleset)
    (delete (rule-name rule) (ruleset-rule-list ruleset))))

(defmethod sort-rule-into-ruleset ((ruleset ruleset) rule)
  (let ((rule-list (ruleset-rule-list ruleset)))
    (if rule-list
	(nconc rule-list (list (rule-name rule)))
      (setf (ruleset-rule-list ruleset) (list (rule-name rule))))))

(defmethod solve-using-ruleset ((ruleset ruleset) form)
  (solve-using-ruleset-rules form (ruleset-rule-list ruleset) ruleset))

(defvar *default-ruleset* 'ruleset)

;;; EOF
