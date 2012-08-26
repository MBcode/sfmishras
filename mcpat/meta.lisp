;;; -*-  Mode: lisp; Syntax: Ansi-Common-Lisp; Package: MCPAT; Base: 10 -*-

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

;; $Id: meta.lisp 97 2006-08-31 06:00:10Z smishra $

;; The meta-circular part of the meta-circular pattern compiler. The
;; hand-crafted rules are used to generate standard rule definitions
;; for the compiler patterns.

(in-package :mcpat)

(in-ruleset :|internal-rule-compiler|)

(defrule binding-compiler
    ((compile ?var-pattern ?input ?bound)
     :constraints ((not (eql ?var-pattern '?))
		   (pattern-variable-p ?var-pattern)))
  "Constructs a form for a binding, based on whether the binding has been
previously encountered."
  (if (member ?var-pattern ?bound)
      (values `((equal ,?var-pattern ,?input)) ?bound)
    (values `((setq ,?var-pattern ,?input) ,t)
	    (cons ?var-pattern ?bound))))

(defrule anonymous-binding-compiler
    ((compile ?var-pattern ?input ?bound)
     :constraints ((symbolp ?var-pattern)
		   (string= (symbol-name ?var-pattern) "?")))
  "Constructs a form for an anonymous binding. The matching input is 
ignored."
  (values `((declare (ignore ,?input))
	    t)
	  ?bound))

(defrule constant-compiler
    ((compile ?atom-pattern ?input ?bound)
     :constraints ((atom ?atom-pattern)))
  "Constructs a form for a constant. As compared to the bootstrap method,
this rule checks for some basic object classes for efficiency."
  (values (cond ((stringp ?atom-pattern)
		 `((and (stringp ,?input) (string= ,?atom-pattern ,?input))))
		((numberp ?atom-pattern)
		 `((and (numberp ,?input) (= ,?atom-pattern ,?input))))
		((symbolp ?atom-pattern)
		 `((eq ',?atom-pattern ,?input)))
		(t `((equal ',?atom-pattern ,?input))))
	  ?bound))

(defmacro with-compilation ((compiled-var subform input-var old-bindings-var
			     &optional (new-bindings-var old-bindings-var))
			    &body body)
  "A macro useful for handling subform compilation. It sets up a solve
statement, gathers the results, tests for successful solution, and binds
the compiled form to the value of COMPILED-VAR and the new bindings to
that of ?BOUND. In this environment it executes the remainder of the body."
  `(multiple-value-bind (,compiled-var ,new-bindings-var)
       (solve (list 'compile ,subform ,input-var ,old-bindings-var))
     ,@body))

(defrule segment-compiler
    ((compile (?segment-pattern . ?rest-pattern) ?input ?bound)
     :constraints ((consp ?segment-pattern)
		   (symbolp (car ?segment-pattern))
		   (string= (symbol-name (car ?segment-pattern)) "?*")
		   (pattern-variable-p (cadr ?segment-pattern))))
  "Constructs a match expression for a segment variable. A segment 
variable is represented as (?* ?var)."
  (let* ((var (cadr ?segment-pattern))
	 (var-bound-p (member var ?bound))
	 (?bound (adjoin var ?bound)))
    (with-compilation (compiled-rest ?rest-pattern ?input ?bound)
      (if var-bound-p
	  (values `((loop for rest-input = ,?input then (cdr rest-input)
			for rest-var-value on ,var
			always (and rest-input
				    (equal (car rest-input) (car rest-var-value)))
			finally (return
				  (unless (and rest-var-value (null rest-input))
				    (let ((,?input rest-input))
				      ,@compiled-rest)))))
		  ?bound)
	(values `((block ,var
		    (setq ,var
		      (loop for rest-input on ,?input
			  when (let ((,?input rest-input))
				 ,@compiled-rest)
			  return ,var
			  collect (car rest-input) into ,var
			  finally (if (let ((,?input rest-input))
					,@compiled-rest)
				      (return ,var)
				    (return-from ,var nil))))
		    ,t))
		?bound)))))

(defun binding-alist-lookup (binder-key input)
  `(multiple-value-bind (result error)
       (ignore-errors (assoc ,binder-key ,input :test #'string-equal))
     (if error
	 (return-from ?@ nil)
       result)))

(defun make-alist-explicit-binder (binder input bound)
  (let ((binder-key (string (car binder)))
	(binder-pattern (cdr binder)))
    (with-compilation (compiled-pattern binder-pattern input bound)
      (values `(let* ((binding ,(binding-alist-lookup binder-key input))
		      (,input (cdr binding)))
		 (unless ,(if (cdr compiled-pattern)
			      `(and ,@compiled-pattern)
			    (car compiled-pattern))
		   (return-from ?@ nil)))
	      bound))))

(defun make-alist-key-test (key input)
  `(let ((binding ,(binding-alist-lookup key input)))
     (unless binding
       (return-from ?@ nil))))

(defun make-alist-bound-var-binder (binder input)
  (let ((binder-key (subseq (symbol-name binder) 1)))
    `(let ((binding ,(binding-alist-lookup binder-key input)))
       (unless (equal ,binder (cdr binding))
	 (return-from ?@ nil)))))

(defun make-alist-strict-set-binder (binder input)
  (let ((binder-key (subseq (symbol-name binder) 1)))
    `(let ((binding ,(binding-alist-lookup binder-key input)))
       (if binding
	   (setq ,binder (cdr binding))
	 (return-from ?@ nil)))))

(defun make-alist-permissive-set-binder (binder input)
  (let ((binder-key (subseq (symbol-name binder) 1)))
    `(let ((binding ,(binding-alist-lookup binder-key input)))
       (setq ,binder (cdr binding)))))

(defun make-alist-pattern-binders (binders input bound strictp)
  (let ((result nil))
    (dolist (binder binders)
      (cond ((consp binder)
	     (multiple-value-bind (matcher new-bound)
		 (make-alist-explicit-binder binder input bound)
	       (push matcher result)
	       (setq bound new-bound)))
	    ((and (not (pattern-variable-p binder)) (or (symbolp binder) (stringp binder)))
	     (make-alist-key-test binder input))
	    ((not (pattern-variable-p binder))
	     (error "Invalid entry ~S in alist binders ~S" binder binders))
	    ((member binder bound)
	     (push (make-alist-bound-var-binder binder input) result))
	    (strictp
	     (push (make-alist-strict-set-binder binder input) result)
	     (push binder bound))
	    (t (push (make-alist-permissive-set-binder binder input) result)
	       (push binder bound))))
    (values (nreverse result) bound)))

(defrule alist-pattern-compiler
    ((compile ?alist-pattern ?input ?bound)
     :constraints ((consp ?alist-pattern)
		   (symbolp (car ?alist-pattern))
		   (let ((alist-string (symbol-name (car ?alist-pattern))))
		     (and (>= (length alist-string) 2)
			  (char= (char alist-string 0) #\?)
			  (char= (char alist-string 1) #\@)))
		   (cdr ?alist-pattern)
		   (every #'(lambda (entry)
			      (or (pattern-variable-p entry) 
				  (and (consp entry) (atom (car entry)))
				  (symbolp entry)
				  (string entry)))
			  (cdr ?alist-pattern))))
  "Constructs an alist compilation form with (?@ e1 ... en) as the
input pattern. The binding proceeds thus: the current input is assumed
to be a valid alist. Each ``ei'' must be either a pattern form or a 
binding description. If ``ei'' is a pattern variable of the form
``?vari'', we use a STRING-EQUAL test to find a key matching ``vari''
in the alist. If a binding is not found, then the binding gets a value
of NIL. Replacing ``?@'' with ``?@!'' produces a failure if no match
is found. ``ei'' as a binding description can be either a cons cell or
a string or symbol. If a string or symbol, that is interpreted as a
requirement that the corresponding key be present in the alist. If it
is a cons cell, then the car is taken to be the key, and the cdr a
pattern that is matched against the value of the key in the alist. If
this pattern match fails, the rule is considered to have failed."
  (multiple-value-bind (rule-forms ?bound)
      (make-alist-pattern-binders (cdr ?alist-pattern) ?input ?bound
				  (string= (symbol-name (car ?alist-pattern)) "?@!"))
    (values
     `((block ?@
	 ,.rule-forms
	 t))
     ?bound)))

(defrule compound-compiler
    ((compile ?compound-pattern ?input ?bound)
     :constraints ((consp ?compound-pattern)))
  "Constructs a match expression for a list pattern, using the match
expressions obtained for the CAR and CDR of the pattern."
  (with-compilation (compiled-car (car ?compound-pattern) ?input ?bound)
    (with-compilation (compiled-cdr (cdr ?compound-pattern) ?input ?bound)
      (values `((and (consp ,?input)
		     (let ((,?input (car ,?input)))
		       ,@compiled-car)
		     (let ((,?input (cdr ,?input)))
		       ,@compiled-cdr)))
	      ?bound))))

;;; EOF
