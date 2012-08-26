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

;; $Id: test.lisp 97 2006-08-31 06:00:10Z smishra $

(in-package :mcpat)
(in-ruleset :test-rules encoded-ruleset)
(clear-ruleset :test-rules)

;;; Simple variabled patterns

(defparameter *sexp*
  '(envelope ()
    (body ()
     (container ()
      (part1 ()
	     (slot11 () val11)
	     (slot12 () (ref () x)
		     (ref () y)
		     (ref () z)))))))

(defparameter *result* '((container (part1 slot11 val11 slot12 (x y z)))))

(defrule my-envelope
    ((envelope () ?body))
  (solve ?body))

(defrule my-body
    ((body () . ?body))
  (mapcar #'(lambda (object)
	      (let ((object-name (car object))
		    (object-content (cddr object)))
		(solve `(object ,object-name ,object-content))))
	  ?body))

(defrule your-container
    ((object container ?content))
  (cons 'container
	(solve `(body () ,@?content))))

(defrule your-part
    ((object ?part ?content)
     :constraints ((member ?part '(part1 part2))))
  (cons ?part
	(apply #'nconc (solve `(slot-values ,?content)))))

(defrule my-slot-values
    ((slot-values ?slot-value-pairs))
  (mapcar #'(lambda (slot-value-exp)
	      (solve `(slot ,(car slot-value-exp) ,(cddr slot-value-exp))))
	  ?slot-value-pairs))

(defrule your-slot-value
    ((slot ?slot-name ?slot-value))
  (list ?slot-name (if (cdr ?slot-value)
		       (solve `(array ,?slot-value ref))
		     (car ?slot-value))))

(defrule my-array
    ((array ?array-content ?indicator))
  (with-rules ((my-array-deconstructor
		((array-element ?indicator () ?value))
		?value))
    (mapcar #'(lambda (element)
		(solve `(array-element ,@element)))
	    ?array-content)))

(fresh-line)
(princ "Testing matcher... ")
(princ (if (equal (solve *sexp*) *result*) "Success." "Failure."))

(fresh-line)
(princ "Testing matcher with tracing... ")
(trace-mcpat :all)
(solve *sexp*)

(fresh-line)
(princ "Testing matcher with tracing :test-rules... ")
(untrace-mcpat :all)
(trace-mcpat :test-rules)
(solve *sexp*)

;;; A simple segment match test

(clear-ruleset :test-rules)
(untrace-mcpat :all)

(defparameter *sexp2* '(the-segment a b (a b)))
(defparameter *result2* '(a b))

(defparameter *sexp3* '(the-segment (a b) a b))
(defparameter *result3* '(a b))

(defparameter *sexp4* '((a b) a b))
(defparameter *result4* '(a b))

(defrule pre-segment-matching-rule
    ((the-segment (?* ?segment) ?segment))
  ?segment)

(defrule post-segment-matching-rule
    ((the-segment ?segment (?* ?segment)))
  ?segment)

(defrule tail-segment-matching-rule
    (((a b) (?* ?segment)))
  ?segment)

(fresh-line)
(princ "Testing segment matching binder... ")
(princ (if (equal (solve *sexp2*) *result2*) "Success." "Failure."))

(fresh-line)
(princ "Testing segment matching... ")
(princ (if (equal (solve *sexp3*) *result3*) "Success." "Failure."))

(fresh-line)
(princ "Testing tail segment matching... ")
(princ (if (equal (solve *sexp4*) *result4*) "Success." "Failure."))

;;; Anonymous pattern variables

(clear-ruleset :test-rules)

(defrule anonymous-pattern
    ((the-segment ? ?x (? ?x)))
  ?x)

(fresh-line)
(princ "Testing anonymous matching... ")
(princ (if (eql (solve *sexp2*) 'b) "Success" "Failure"))

(fresh-line)
(princ "Testing anonymous matching failure... ")
(handler-case (progn (solve *sexp3*) (princ "Failure"))
  (mcpat:mcpat-solve-failure () (princ "Success")))

;;; Alist pattern tests

(clear-ruleset :test-rules)

(defparameter *sexp5*
    '(:alist-permissive 1 ((a . 1) (b 2) (c . 3) (d . 4)) 2 3 4))

(defparameter *sexp6*
    '(:alist-strict 1 ((a . 1) (b 3) (c . 3) (d . 4)) 2 3 4))

(defparameter *sexp7*
    '(:complex-alist-value ((a . 1) (b a b (c)) (c 2)) ((c . 2) (d . 3))))

(mcpat:defrule test-alist-binding-permissive
    ((:alist-permissive ?a (?@ ?a ?b (c . 3) ?e (b . ?b)) (?* ?b) ?c ?))
  (list ?a ?b ?c ?e))

(mcpat:defrule test-alist-binding-strict
    ((:alist-strict ?a (?@! ?a (c . 3) ?e) ? ?c ?))
  (list ?a ?c ?e))

(mcpat:defrule test-alist-binding-complex
    ((:complex-alist-value (?@ (b a ?x (?* ?y)) ?c) (?@ (c . ?c2) d))
     :constraints ((eql (car ?c) ?c2)))
  (list ?x ?y))

(fresh-line)
(princ "Testing alist permissive binding... ")
(princ (if (equal (solve *sexp5*) '(1 (2) 3 nil)) "Success" "Failure"))

(fresh-line)
(princ "Testing alist strict binding failure... ")
(handler-case (progn (solve *sexp6*) (princ "Failure"))
  (mcpat:mcpat-solve-failure () (princ "Success")))

(fresh-line)
(princ "Testing alist complex binding... ")
(princ (if (equal (solve *sexp7*) '(b ((c)))) "Success" "Failure"))

;;; Simple priority match test

(in-ruleset :test-priority priority-ruleset)
(clear-ruleset :test-priority)

(defrule low-priority-rule
    ((?x ?y))
  (list ?x ?y))

(defrule high-priority-rule
    ((a ?y))
  ?y)

(defparameter *sexp8* '(b a))
(defparameter *sexp9* '(a b))

(fresh-line)
(princ "Testing high priority test match failure...")
(princ (if (equal (solve *sexp8*) *sexp8*) "Success" "Failure"))

(fresh-line)
(princ "Testing high priority test match success...")
(princ (if (equal (solve *sexp9*) 'b) "Success" "Failure"))

;;; EOF
