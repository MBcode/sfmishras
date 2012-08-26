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

;; $Id: rule-compiler.lisp 97 2006-08-31 06:00:10Z smishra $

;; Meta-circular pattern compiler. Hand-crafted rules form the basis for
;; the rule compiler, which can then be used to create arbitrary rules and
;; even customize the rule compilation.

(in-package :mcpat)

;;; The compiler rule-set

;; First we define the rule set. Then we describe each of the basic rules
;; and hand-craft an expansion. These hand-crafted expansions are only
;; stop-gap measures to be replaced with a rule generated through normal
;; expansion.

(in-ruleset :|internal-rule-compiler|)
(clear-ruleset :|internal-rule-compiler|)
(ensure-ruleset :|external-rule-compiler|)

;; For unseen variables

#|
(defrule binding-compiler
    ((compile ?var-pattern ?input ?bound)
     :constraints ((pattern-variable-p ?var-pattern)))
  (if (member ?var-pattern ?bound)
      (values `((equal ,?var-pattern ,?input) ?bound))
    (values `((setq ,?var-pattern ,?input) t)
	    (cons ?var-pattern ?bound))))
|#

(defun binding-compiler (pattern)
  "Constructs a form for a binding, based on whether the binding has been
previously encountered."
  (if (and (consp pattern)
	   (eql (car pattern) 'compile))
      (destructuring-bind (var-pattern input bound) (cdr pattern)
	(if (pattern-variable-p var-pattern)
	    (if (member var-pattern bound)
		(values `((equal ,var-pattern ,input)) 
			bound)
	      (values `((setq ,var-pattern ,input) ,t)
		      (cons var-pattern bound)))
	  (throw pattern nil)))
    (throw pattern nil)))

(store-rule 'binding-compiler '(compile ?var-pattern ?input ?bound) 'binding-compiler)

;; For seen variables and other atoms

#|
(defrule constant-compiler
    ((compile ?atom-pattern ?input ?bound)
     :constraints ((atom ?atom-pattern)))
  (values `((equal ',?atom-pattern ,?input) ?bound)))
|#

(defun constant-compiler (pattern)
  "Constructs a form for a constant."
  (if (and (consp pattern)
	   (eql (car pattern) 'compile))
      (destructuring-bind (atom-pattern input bound) (cdr pattern)
	(if (atom atom-pattern)
	    (values `((equal ',atom-pattern ,input)) bound)
	  (throw pattern nil)))
    (throw pattern nil)))

(store-rule 'constant-compiler 'compile 'constant-compiler)

;; For compound expressions

#|
(defrule compound-compiler
    ((compile ?compound-pattern ?input ?bound)
     :constraints ((consp ?compound-pattern)))
  (destructuring-bind (compiled-car car-bindings)
      (solve `(compile ,(car ?compound-pattern) ,?input ,?bound))
    (destructuring-bind (compiled-cdr cdr-bindings)
	(solve `(compile ,(cdr ?compound-pattern) ,?input ,car-bindings))
      (values `((and
		 (consp ,?input)
		 (let ((,?input (car ,?input)))
		   ,@compiled-car)
		 (let ((,?input (cdr ,?input)))
		   ,@compiled-cdr)))
	      cdr-bindings))))
|#

(defun compound-compiler (pattern)
  "Constructs a match expression for a list pattern, using the match
expressions obtained for the CAR and CDR of the pattern."
  (if (and (consp pattern)
	   (eql (car pattern) 'compile))
      (destructuring-bind (compound-pattern input bound) (cdr pattern)
	(if (consp compound-pattern)
	    (multiple-value-bind (compiled-car car-bindings)
		(solve `(compile ,(car compound-pattern) ,input ,bound))
	      (multiple-value-bind (compiled-cdr cdr-bindings)
		  (solve `(compile ,(cdr compound-pattern) ,input ,car-bindings))
		(values `((and (consp ,input)
			       (let ((,input (car ,input)))
				 ,@compiled-car)
			       (let ((,input (cdr ,input)))
				 ,@compiled-cdr)))
			cdr-bindings)))
	  (throw pattern nil)))
    (throw pattern nil)))

(store-rule 'compound-compiler '(compile ?compound-pattern ?input ?bound) 'compound-compiler)

;;; with-rules macrolet

(defun %subrule-form (subrule bind-vars)
  (destructuring-bind (name (pattern &key constraints) &body body) subrule
    (multiple-value-bind (lambda-form documentation)
	(compile-rule name pattern constraints body bind-vars t)
      `(make-rule :name ',name :pattern ',pattern :function #',lambda-form
		  :documentation ,documentation))))

(defun %with-rules-expansion-form (subrule-exps body)
  `(let ((*dynamic-rules* (list* ,.subrule-exps *dynamic-rules*)))
    ,@body))

(defun %with-rules-form (bind-vars)
  "Constructs a macrolet form for WITH-RULES, used for introducing subrules
in the course of solving for an expression."
  (let ((macro-name (intern "WITH-RULES")))
    `(,macro-name ((&rest subrule-forms) &body body)
      (let ((subrule-exps (mapcar #'(lambda (subrule)
				      (%subrule-form subrule ',bind-vars))
				  subrule-forms)))
	(%with-rules-expansion-form subrule-exps body)))))

;;; defrule

(defun parse-body-components (raw-body)
  "Separates documentation and declarations from the raw input body. This
is based on my understanding of the standard, rather than a thorough study.
At most one documentation and a list of declaration forms are assumed to be
allowed at the start of the body. The return values are the documentation
string, the declarations, and the remaining list of executable forms."
  (loop with documentation = nil
	for rest-body on raw-body
	for form = (car rest-body)
	if (and (consp form) (eql (car form) 'declare))
	  collect form into declarations
	else if (and (stringp form) (cdr rest-body) (null documentation))
          do (setq documentation form)
	else return (values documentation declarations rest-body)))

(defun inline-expand-with-rules-form (form bind-vars)
  (let ((subrule-forms (cadr form))
	(with-rules-body (cddr form)))
    (when (and subrule-forms with-rules-body)
      (let ((subrule-exps
	     (mapcar #'(lambda (exp)
			 (or (ignore-errors (%subrule-form exp bind-vars))
			     (return-from inline-expand-with-rules-form nil)))
		     subrule-forms)))
	`(let ((*dynamic-rules* (list* ,.subrule-exps *dynamic-rules*)))
	  ,@(attempt-with-rules-expansion with-rules-body bind-vars))))))

(defun attempt-with-rules-expansion (body bind-vars)
  (let ((with-rules (intern "WITH-RULES")))
    ;; Locate an expression subtree headed by WITH-RULES
    (labels ((%expand-loop (form)
	       ;; Recursive step to locate WITH-RULES expressions
	       ;; Conses only when necessary
	       (if (consp form)
		   (if (eql (car form) with-rules)
		       (or (inline-expand-with-rules-form form bind-vars)
			   form)
		     (let* ((form-car (car form))
			    (form-cdr (cdr form))
			    (result-car (if (consp form-car)
					    (%expand-loop form-car)
					  form-car))
			    (result-cdr (if (consp form-cdr)
					    (%expand-loop form-cdr)
					  form-cdr)))
		       (if (and (eql form-car result-car)
				(eql form-cdr result-cdr))
			   form
			 (cons result-car result-cdr))))
		 (error "Should not have seen non-cons form ~S here." form))))
      (%expand-loop body))))

(defun compile-rule (name pattern constraints raw-body known-bindings lambda-p)
  "The primary pattern compilation function. It invokes the compiler rules
to compile a pattern, and returns forms corresponding to the rule function
definition, the documentation, and the storage key."
  (let ((input-var 'input))
    (multiple-value-bind (compiled-test bind-vars)
	(with-search-rulesets (:|external-rule-compiler| :|internal-rule-compiler|)
	  (solve `(compile ,pattern ,input-var ,known-bindings)))
      (let ((predicate-form (append compiled-test constraints))
	    (with-rules-form (%with-rules-form bind-vars))
	    (definer-form (if lambda-p
			      '(lambda)
			    `(defun ,name))))
	(multiple-value-bind (documentation declarations body)
	    (parse-body-components raw-body)
	  (values
	   `(,@definer-form
	     (,input-var &aux ,@(set-difference bind-vars known-bindings))
	     ,.(when documentation (list documentation))
	     ,.declarations
	     (macrolet (,with-rules-form)
	       (if ,(if (cdr predicate-form)
			`(and ,@predicate-form)
		      (car predicate-form))
		   (progn ,@(attempt-with-rules-expansion body bind-vars))
		 (throw ,input-var nil))))
	   documentation))))))

(defun parse-name-form (name-form)
  (if (atom name-form)
      (values name-form nil)
    (destructuring-bind (name &key ruleset) name-form
      (values name ruleset))))

(defmacro defrule (name-form (pattern &key constraints) &body body)
  "The rule definition macro."
  (multiple-value-bind (name ruleset-name)
      (parse-name-form name-form)
    (multiple-value-bind (function-form documentation)
	(compile-rule name pattern constraints body nil nil)
      `(progn
	 ,function-form
	 (store-rule ',name ',pattern #',name
		     ,@(when ruleset-name (list :ruleset ruleset-name))
		     ,@(when documentation (list :documentation documentation)))
	 ',name))))

(defmacro defrule-compiler (name (pattern &key constraints) &body body)
  "The compiler rule definition macro. If DEFRULE is analogous to DEFUN,
then DEFRULE-COMPILER is analogous to DEFMACRO. It implicitly specifies
the appropriate ruleset for storing the rule."
  `(defrule
    (,name :ruleset :|external-rule-compiler|)
    (,pattern :constraints ,constraints)
    ,@body))

;;; For compiling the meta-circular matcher

(defun construct-binary-pathname (src-pathname bin-directory-name)
  (if bin-directory-name
      (merge-pathnames
       (make-pathname :directory (list :relative bin-directory-name))
       src-pathname)
    src-pathname))

(defun meta-binary-pathname (src-pathname bin-directory)
  (compile-file-pathname
   (construct-binary-pathname src-pathname bin-directory)))

(defun ensure-binary-directory (src-directory bin-directory-name)
  (let ((bin-directory 
	 (construct-binary-pathname src-directory bin-directory-name)))
    (unless (probe-file bin-directory)
      #+allegro
      (excl:make-directory bin-directory))))

(defun file-newer-p (file1 file2)
  (> (or (file-write-date file1) 0) (or (file-write-date file2) 0)))

(defun meta-pathnames (src-directory bin-directory)
  (ensure-binary-directory src-directory bin-directory)
  (let ((meta-src
	 (make-pathname :name "meta" :type "lisp" :defaults src-directory)))
    (values meta-src (meta-binary-pathname meta-src bin-directory))))

(defun compile-meta-rules-if-needed (src-directory &optional bin-directory)
  (multiple-value-bind (src-pathname bin-pathname)
      (meta-pathnames src-directory bin-directory)
    (when (file-newer-p src-pathname bin-pathname)
      ;; Do it once, generate the meta rules from the hand-crafted rules
      (compile-file src-pathname :output-file bin-pathname)
      (clear-ruleset :|internal-rule-compiler|)
      (load bin-pathname)
      ;; Do it again, generate the meta rules from the meta rules
      (compile-file src-pathname :output-file bin-pathname)
      (clear-ruleset :|internal-rule-compiler|)
      (load bin-pathname))
    bin-pathname))

(defun load-meta-rules (src-directory &optional bin-directory-name)
  (let ((bin-pathname
	 (compile-meta-rules-if-needed src-directory bin-directory-name)))
    (clear-ruleset :|internal-rule-compiler|)
    (load bin-pathname)))

;;; EOF
