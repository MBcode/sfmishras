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

;; $Id: rule-engine.lisp 97 2006-08-31 06:00:10Z smishra $

(in-package :mcpat)

;; The function for applying rules to objects.

;;; Debugging

(defstruct tracer
  ;; TRACED can be a list, or the symbol :ALL
  ;; EXCEPTIONS is always a list
  ;; EXCEPTIONS must be empty if TRACED is a list
  ;; EXCEPTIONS may be non-empty if TRACED is :ALL
  traced
  exceptions)

(defvar *mcpat-traces* (make-tracer))
(defvar *trace-stream* *standard-output*)
(defvar *current-trace-stream*)

(defun trace-mcpat (&optional what)
  ;; Tracing :ALL sets TRACED, unsets EXCEPTIONS
  ;; Tracing anything while :ALL is traced causes WHAT to be subtracted from EXCEPTIONS
  ;; Otherwise WHAT is accumulated into TRACED
  (cond ((null what))
	((eql what :all)
	 (setf (tracer-traced *mcpat-traces*) :all)
	 (setf (tracer-exceptions *mcpat-traces*) nil))
	((eql (tracer-traced *mcpat-traces*) :all)
	 (setf (tracer-exceptions *mcpat-traces*)
	   (if (listp what)
	       (set-difference (tracer-exceptions *mcpat-traces*) what)
	     (remove what (tracer-exceptions *mcpat-traces*)))))
	((consp what)
	 (setf (tracer-traced *mcpat-traces*) (union (tracer-traced *mcpat-traces*) what)))
	(t
	 (pushnew what (tracer-traced *mcpat-traces*))))
  *mcpat-traces*)

(defun untrace-mcpat (&optional (what :all))
  ;; Untracing :ALL unsets TRACED and EXCEPTIONS
  ;; Untracing anything while :ALL is traced causes WHAT to be accumulated in EXCEPTIONS
  ;; Otherwise WHAT is subtracted from TRACED
  (cond ((null what))
	((eql what :all)
	 (setf (tracer-traced *mcpat-traces*) nil)
	 (setf (tracer-exceptions *mcpat-traces*) nil))
	((eql (tracer-traced *mcpat-traces*) :all)
	 (setf (tracer-exceptions *mcpat-traces*)
	   (if (listp what)
	       (union (tracer-exceptions *mcpat-traces*) what)
	     (adjoin what (tracer-exceptions *mcpat-traces*)))))
	((consp what)
	 (setf (tracer-traced *mcpat-traces*) 
	   (set-difference (tracer-traced *mcpat-traces*) what)))
	(t (setf (tracer-traced *mcpat-traces*) (remove what (tracer-traced *mcpat-traces*)))))
  *mcpat-traces*)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun trace-active-p-form (tracers)
    `(and (tracer-traced *mcpat-traces*)
	  ;; Trace is allowed
	  (or (eql (tracer-traced *mcpat-traces*) :all)
	      ,@(mapcar #'(lambda (tracer)
			    `(member ,tracer (tracer-traced *mcpat-traces*)))
			tracers))
	  ;; Trace is not in exceptions
	  (not (or ,@(mapcar #'(lambda (tracer)
				 `(member ,tracer (tracer-exceptions *mcpat-traces*)))
			     tracers))))))

(defmacro with-trace ((&rest tracers) &body body)
  `(when ,(trace-active-p-form tracers)
     (let ((*current-trace-stream* *trace-stream*))
       (fresh-line *current-trace-stream*)
       (write-string "**MCPAT: " *current-trace-stream*)
       ,@body)))

(defmacro with-trace-if ((&rest tracers) then-form else-form)
  `(cond (,(trace-active-p-form tracers)
	  (let ((str (make-array 0 :fill-pointer t :adjustable t :element-type 'character))
		(*current-trace-stream* *trace-stream*))
	    (multiple-value-prog1 (with-output-to-string (*current-trace-stream* str)
				    ,then-form)
	      (fresh-line *current-trace-stream*)
	      (write-string "**MCPAT: " *current-trace-stream*)
	      (write-string str *current-trace-stream*))))
	 (t ,else-form)))

(defun trace-format (control-string &rest args)
  (apply #'format *current-trace-stream* control-string args))

;;; Solver

(defun solve-using-rule (form rule ruleset-name)
  "Applies a single rule to a single form."
  (with-trace (:match-attempt ruleset-name)
    (trace-format "Trying ~S against ~S" form rule))
  (with-trace-if (:match-success ruleset-name)
    (let ((result (multiple-value-list (funcall (rule-function rule) form))))
      (trace-format "~S matched ~S -->~{ ~S~}" form rule result)
      (values-list result))
    (funcall (rule-function rule) form)))

(defun solve-using-ruleset-rules (form candidates ruleset)
  (let ((table (ruleset-rules ruleset)))
    (map nil #'(lambda (rule-name)
		 (let ((rule (gethash rule-name table)))
		   (catch form
		     (return-from solve-using-ruleset-rules
		       (solve-using-rule form rule (ruleset-name ruleset))))))
	 candidates))
  (throw ruleset nil))

(defgeneric solve-using-ruleset (ruleset form)
  )

(defun solve (form &optional (if-solve-failed :error) default-value)
  "Applies the rules in the search rulesets to the given input
form. It gives rules in *dynamic-rules* precedence over the rulesets.
The arguments if-solved-failed and default-value control solver behavior
on failure. The default behavior has if-solved-failed set to :error, so
solve throws an error of type mcpat-solve-failure if no matching rule is
found. If :soft or nil instead, then solve returns nil. Finally, a value
of :default for if-solved-failed causes default-value to be returned."
  (flet ((%solve-using-ruleset (ruleset)
	   (catch ruleset
	     (return-from solve (solve-using-ruleset ruleset form))))
	 (%solve-with-dynamic-rule (rule)
	   (catch form
	     (return-from solve (solve-using-rule form rule :dynamic))))
	 (%solve-fail ()
	   (ecase if-solve-failed
	     (:error (error 'mcpat-solve-failure :form form))
	     ((:soft nil) nil)
	     (:default default-value))))
    (restart-case
	(or (dolist (rule *dynamic-rules*)
	      (%solve-with-dynamic-rule rule))
	    (if *search-rulesets*
		(dolist (ruleset *search-rulesets*)
		  (%solve-using-ruleset ruleset))
	      (%solve-using-ruleset *ruleset*))
	    (%solve-fail))
      (solve-return-default ()
	  :report "Return the default value."
	default-value)
      (solve-return-value (value)
	  :report "Enter a value to return."
	  :interactive read
	value))))

;;; EOF
