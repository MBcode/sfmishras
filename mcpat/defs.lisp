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

;; $Id: defs.lisp 97 2006-08-31 06:00:10Z smishra $

(in-package :mcpat)

;; The Meta-Circular Pattern Matcher is a simple rule engine that
;; emphasizes simplicity, efficiency and expressiveness. It is far
;; from a general problem solver, capable only of matching a single
;; form at a time against its rule base. (Most rule engines have a
;; sophisticated assertion management system that allows complex rules
;; to match against multiple assertions.) In that sense, MPCAT is more
;; like a rule based computation specifier than a rule engine.

;; A rule consists of an antecedent and a consequent. The antecedent
;; is a pattern in a specialized language, and the consequent is an
;; arbitrary lisp form that executes only if the antecedent matches
;; successfully. The pattern variables are bound as lexical variables
;; in the consequent body, allowing for its straightforward expression
;; and efficient execution.

;; The pattern matcher is meta-circular in that the rule compiler is
;; expressed in terms of rules. This allows for extending the compiler
;; to handle new rule patterns very easily.

;; Finally, the rules are organized in package-like structures called
;; rulesets. While calling the pattern matcher, a program can specify
;; the order in which rulesets are to be called, and rules are then
;; tested in the corresponding order.

;; There are two other important restrictions on a rule: the first
;; element of a pattern cannot be a pattern variable, as this
;; interferes with the efficient retrieval of rules. And a rule may
;; return only a single value, so multiple values must be returned as
;; a list.

;;;----------------------------------------
;;; Condition classes

(define-condition mcpat-error (error)
  ())

(define-condition mcpat-solve-error (mcpat-error)
  ((form :initarg :form))
  (:report (lambda (c stream)
	     (with-slots (form) c
	       (format stream "Solve error on form ~S" form)))))

(define-condition mcpat-solve-failure (mcpat-solve-error)
  ()
  (:report (lambda (c stream)
	     (with-slots (form) c
	       (format stream "Solve failed on form ~S" form)))))

;;;----------------------------------------
;;; Rulesets

(defvar *all-rulesets* nil)
(defvar *ruleset*)
(defvar *search-rulesets* nil)

(defclass abstract-ruleset ()
  ((name :reader ruleset-name :initform (error "Ruleset name is required.") :initarg :name)
   (rules :accessor ruleset-rules :initform (make-hash-table))
   (rule-count :accessor ruleset-rule-count :initform 0))
  (:documentation "The abstract ruleset. This ruleset should not be
instantiated and used, as it does not make any commitments to how
rules are stored and retrieved."))

(defmethod print-object ((object abstract-ruleset) stream)
  (print-unreadable-object (object stream)
    (format stream "Ruleset: ~A" (ruleset-name object))))

(defun ensure-ruleset (ruleset &optional (class *default-ruleset*))
  "Ensures that the argument is a ruleset, or names a ruleset. If
neither, a new ruleset is instantiated."
  (declare (special *default-ruleset*))
  (cond ((typep ruleset 'abstract-ruleset) ruleset)
	(t (assert (keywordp ruleset) (ruleset)
	     "Ruleset ~S must be named using a keyword." ruleset)
	   (let ((old-ruleset (find-ruleset ruleset)))
	     (if old-ruleset
		 (values old-ruleset t)
	       (values (store-ruleset (make-instance class :name ruleset)) nil))))))

(defun find-ruleset (name)
  (find name *all-rulesets* :key #'ruleset-name))

(defun store-ruleset (ruleset)
  (push ruleset *all-rulesets*)
  ruleset)

(defgeneric clear-ruleset (ruleset)
  (:method-combination progn))

(defmethod clear-ruleset progn ((name symbol))
  "Deletes all rules from a ruleset."
  (clear-ruleset (find-ruleset name)))

(defmethod clear-ruleset progn ((ruleset abstract-ruleset))
  (clrhash (ruleset-rules ruleset))
  (setf (ruleset-rule-count ruleset) 0))

(defun clear-all-rulesets ()
  (dolist (ruleset *all-rulesets*)
    (unless (or (eql (ruleset-name ruleset) :|external-rule-compiler|)
		(eql (ruleset-name ruleset) :|internal-rule-compiler|))
      (clear-ruleset ruleset))))

(defmacro in-ruleset (name &optional (class *default-ruleset*))
  "The intended use of this macro is analogous to CL:IN-PACKAGE. It is
intended for use at the head of a rule file for specifying the default
ruleset of the rules defined. Note the :RULESET keyword in the DEFRULE
macro for specifying a different ruleset."
  (declare (special *default-ruleset*))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (let ((ruleset-exists-p nil))
       (multiple-value-setq (*ruleset* ruleset-exists-p) (ensure-ruleset ,name ',class))
       (values *ruleset* ruleset-exists-p))))

(defmacro with-search-rulesets (rulesets &body body)
  "This macro specifies the order of the rulesets to search when
solving for a particular expression. It appends the specified
rulesets to the present list."
  (let ((search-list-form
	 (if (eql (car rulesets) :absolute)
	     `(list ,.(mapcar #'(lambda (ruleset)
				  `(or (find-ruleset ,ruleset)
				       (error "Could not find ruleset ~S" ,ruleset)))
			      (cdr rulesets)))
	   `(list* ,.(mapcar #'(lambda (ruleset)
				 `(or (find-ruleset ,ruleset)
				      (error "Could not find ruleset ~S" ,ruleset)))
			     rulesets)
		   *search-rulesets*))))
    `(let ((*search-rulesets* ,search-list-form))
       ,@body)))

;;;----------------------------------------
;;; Rules

;;; Rule predicates

(defun pattern-variable-p (var)
  (or (eql var '?)
      (and (symbolp var)
	   (let ((var-string (symbol-name var)))
	     (and (> (length var-string) 1)
		  (char= (char var-string 0) #\?)
		  (alphanumericp (char var-string 1)))))))

;;; Rule Management

(defvar *dynamic-rules* nil
  "This variable holds the temporary rules constructed in the course
of solving for some input pattern. As the name suggests, these rules
are lost as soon as the solver exits the stack frame of the defining
rule.")

(defstruct (rule (:print-function (lambda (object stream ignore)
				    (declare (ignore ignore))
				    (print-unreadable-object (object stream)
				      (format stream "Rule: ~A"
					      (rule-name object))))))
  name 
  pattern
  function
  documentation
  sort-data)

(defun store-rule (name pattern function &key (ruleset *ruleset*) documentation)
  "Constructs and stores a rule into a ruleset, replacing the old rule
with the same name if necessary."
  (store-rule-in-ruleset (ensure-ruleset ruleset) name pattern function documentation))

;;; Storage

(defgeneric unsort-rule-from-ruleset (ruleset rule)
  )

(defgeneric sort-rule-into-ruleset (ruleset rule)
  )

(defmethod sort-rule-into-ruleset :before ((ruleset abstract-ruleset) rule)
  (when (rule-sort-data rule)
    (error "Attempted to sort ~S (which already has sort data) into ~S." rule ruleset)))

(defgeneric store-rule-in-ruleset (ruleset name pattern function documentation)
  (:documentation "Create and store a new rule into the given ruleset."))

(defmethod store-rule-in-ruleset ((ruleset abstract-ruleset) name pattern function documentation)
  (let ((rule (make-rule :name name :pattern pattern :function function 
			 :documentation documentation))
	(old-rule (gethash name (ruleset-rules ruleset))))
    ;; First remove the old
    (when old-rule
      (unsort-rule-from-ruleset ruleset old-rule))
    ;; Be able to back out of rule storage on failure
    ;; This may not work if the ruleset itself, and not
    ;; the rule, is in an inconsistent state.
    (let ((success-p nil))
      (unwind-protect
	  (progn
	    (sort-rule-into-ruleset ruleset rule)
	    (setq success-p t))
	(when (and old-rule (not success-p))
	  (sort-rule-into-ruleset ruleset old-rule))))
    ;; Finally add the new
    (unless old-rule
      (incf (ruleset-rule-count ruleset)))
    ;; And increase the rule count
    (setf (gethash name (ruleset-rules ruleset)) rule)
    rule))

;;; EOF
