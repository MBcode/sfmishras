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

;;;;----------------------------------------
;;;; Suite Modification

;;; DEFSUITE and DEFCASE expresson manipulators.

(defun subst-suite-supertype-in-defsuite (form src-name dest-name)
  (assert (eq (car form) 'defsuite))
  (let ((old-supers (third form)))
    (assert (member src-name old-supers))
    (list* 'defsuite (second form) (subst dest-name src-name old-supers) (nthcdr 3 form))))

(defun add-suite-supertype-in-defsuite (form new-name)
  (assert (eq (car form) 'defsuite))
  (let ((old-supers (third form)))
    (list* 'defsuite (second form)
	   (if (member new-name old-supers)
	       old-supers
	     (append old-supers (list new-name)))
	   (nthcdr 3 form))))

(defun remove-suite-supertype-in-defsuite (form name)
  (assert (eq (car form) 'defsuite))
  (let ((old-supers (third form)))
    (list* 'defsuite (second form)
	   (remove name old-supers)
	   (nthcdr 3 form))))

(defun set-suite-supertypes-in-defsuite (form supers)
  (assert (eq (car form) 'defsuite))
  (list* 'defsuite (second form) supers (nthcdr 3 form)))

(defun subst-suite-name-in-defsuite (form src-name dest-name)
  (assert (eq (car form) 'defsuite))
  (assert (eq (cadr form) src-name))
  (list* 'defsuite dest-name (cddr form)))

(defun subst-suite-name-in-defcase (form src-name dest-name)
  (assert (eq (car form) 'defcase))
  (let ((options (cadr form)))
    (cond (options
	   (assert (eq (car options) src-name))
	   (list* 'defcase (cons dest-name (cdr options))
		  (cddr form)))
	  (t form))))

(defun set-suite-fixture-in-defsuite (form fixture setup-body)
  (let ((fixture-option (find-if #'(lambda (item) (and (consp item) (eq (car item) fixture)))
				 form)))
    (cond (fixture-option
	   (setf (cdr fixture-option) setup-body)
	   setup-body)
	  (t (append setup-body `((:setup ,.setup-body)))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun final-modify-without-execute-body (suite-name-var modify-block)
    `((let ((result nil))
	(dolist (exp case-exps)
	  (push exp result))
	(push suite-exp result)
	(push (list 'undefsuite ,suite-name-var) result)
	(return-from ,modify-block result))))

  (defun final-modify-with-execute-body (suite-name-var)
    `((eval (list 'undefsuite ,suite-name-var))
      (eval suite-exp)
      (mapc #'eval (nreverse case-exps))))

  (defun make-with-modify-final-clause (without-execute-form suite-name-var modify-block)
    (cond ((not (constantp without-execute-form))
	   `((cond (,without-execute-form
		    ,@(final-modify-without-execute-body suite-name-var modify-block))
		   (t ,@(final-modify-with-execute-body suite-name-var)))))
	  ((eval without-execute-form)
	   (final-modify-without-execute-body suite-name-var modify-block))
	  (t (final-modify-with-execute-body suite-name-var)))))

(defmacro with-modify-suite-and-cases ((suite-name-var without-execute-form)
				       (suite-exp-var &body suite-body)
				       (case-exp-var &body case-body))
  (let ((modify-block (gensym)))
    `(block ,modify-block
       (flet ((suite-modifier (,suite-exp-var) ,@suite-body ,suite-exp-var)
	      ,@(when case-body
		  `((case-modifier (,case-exp-var) ,@case-body ,case-exp-var))))
	 (with-input-from-string
	     (instr (with-output-to-string (outstr)
		      (write-test ,suite-name-var :stream outstr :recursive nil)))
	   (loop with eof = instr
	       and suite-exp
	       and case-exps = nil
	       for exp = (read instr nil eof)
	       until (eq exp eof)
	       do (ecase (car exp)
		    (defsuite (setq suite-exp (suite-modifier exp)))
		    (defcase ,(if case-body
				  '(push (case-modifier exp) case-exps)
				'(push exp case-exps))))
	       finally ,@(make-with-modify-final-clause without-execute-form 
							suite-name-var modify-block)))))))

(defmacro with-modify-suite ((suite-name-var suite-exp-var without-execute-form) &body body)
  ;; Strictly speaking, this is not necessary. But having name
  ;; differentiation is useful, I think.
  `(with-modify-suite-and-cases (,suite-name-var ,without-execute-form)
     (,suite-exp-var ,@body)
     (ignore-case)))

;;; We cannot just add or remove subclasses to the suite class. The
;;; problem is that not only the suite class, but also the root case
;;; class shall be affected. So the safest way to substitute the
;;; supersuite is to undefine the existing suite, and define a new 
;;; one substituting the supertype. This approach will also be much
;;; more portable than directly manipulating the class objects.

(defun set-suite-supertypes (suite-name-or-class supertypes &key without-execute)
  (let ((suite-name (coerce-to-class-name suite-name-or-class)))
    (with-modify-suite (suite-name exp without-execute)
      (setq exp (set-suite-supertypes-in-defsuite exp supertypes)))))

(defun remove-suite-supertype (suite-name-or-class supertype &key without-execute)
  (let ((suite-name (coerce-to-class-name suite-name-or-class)))
    (when (ignore-errors (subtypep suite-name supertype))
      (with-modify-suite (suite-name exp without-execute)
	(setq exp (remove-suite-supertype-in-defsuite exp supertype))))))

(defun add-suite-supertype (suite-name-or-class supertype &key without-execute)
  (let ((suite-name (coerce-to-class-name suite-name-or-class)))
    (when (or (not (test-suite-p supertype))
	      (not (subtypep suite-name supertype)))
      (with-modify-suite (suite-name exp without-execute)
	(setq exp (add-suite-supertype-in-defsuite exp supertype))))))

(defun subst-suite-supertype (suite-name-or-class old-supertype new-supertype &key without-execute)
  (let ((suite-name (coerce-to-class-name suite-name-or-class)))
    (with-modify-suite (suite-name exp without-execute)
      (setq exp (subst-suite-supertype-in-defsuite exp old-supertype new-supertype)))))

(defun copy-suite (src-name dest-name &key without-execute)
  (let ((copy-forms 
	 (delete 'undefsuite
		 (with-modify-suite-and-cases (src-name t)
		   (exp
		    ;; Define new suite as equivalent of original suite
		    (setq exp (subst-suite-name-in-defsuite exp src-name dest-name)))
		   (exp
		    ;; Define new case as equivalent of original case
		    (setq exp (subst-suite-name-in-defcase exp src-name dest-name))))
		 :key #'car)))
    (if without-execute
	copy-forms
      (dolist (form copy-forms)
	(eval form)))))

(defun rename-suite (old-name new-name &key without-execute)
  (let* ((subsuite-forms nil)
	 (rename-forms
	  (with-modify-suite-and-cases (old-name without-execute)
	    (exp
	     ;; Define new suite as equivalent of original suite
	     (setq exp (subst-suite-name-in-defsuite exp old-name new-name))
	     ;; Substitute suite supertype in all original suite subtypes
	     (setq subsuite-forms
	       (loop for subsuite in (mop:class-direct-subclasses (find-class old-name))
		   nconc (subst-suite-supertype subsuite old-name new-name :without-execute t))))
	    (exp
	     ;; Define new case as equivalent of original case
	     (setq exp (subst-suite-name-in-defcase exp old-name new-name))))))
    (if without-execute
	(nconc rename-forms subsuite-forms)
      (dolist (sub subsuite-forms)
	(eval sub)))))

#|
(defun set-suite-fixture (suite-name fixture body without-execute)
  (with-modify-suite (suite-name exp without-execute)
    (setq exp (set-suite-fixture-in-defsuite exp fixture body))))

(defun set-suite-setup (suite-name setup-body &key without-execute)
  (set-suite-fixture suite-name :setup setup-body without-execute))

(defun set-suite-teardown (suite-name teardown-body &key without-execute)
  (set-suite-fixture suite-name :teardown teardown-body without-execute))
|#

;;; EOF
