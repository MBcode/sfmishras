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

;;; A simple test suite for FReT. The suite depends on support routines in
;;; ../test-support/framework-tests-support.lisp.

;;; This suite defines and runs a test suite that runs some simple tests on
;;; stacks. The purpose is to test suite definition and management in FReT,
;;; so we set up a simple program, a stack, to test through a suite that's
;;; defined for testing.

;;; Effectively, we are testing FReT through FReT.

;;; Suite definitions

(defsuite framework-tests ()
  ()
  (:documentation "The most abstract test in the suite."))

(defsuite suite-execution-tests (framework-tests)
  ((stack-suite :accessor stack-suite))
  (:setup (define-stack-test-suite)
	  (define-stack-consistency-suite))
  (:teardown (undefine-stack-test-suite)
	     (undefine-stack-consistency-suite))
  (:tests (()
	   (ensure (subtypep 'stack-object-tests 'test-suite))
	   (ensure (subtypep 'stack-consistency-tests 'stack-object-tests)))
	  (()
	   (ensure-equal (coerce-to-class-name (last-suite)) 'stack-consistency-tests)))
  (:test (run-stack-object-tests-suite)
	 (run 'stack-object-tests)
	 (setq stack-suite (last-suite))
	 (ensure-type stack-suite stack-object-tests))
  (:test ()
	 (run 'stack-consistency-tests)
	 (ensure-equal (coerce-to-class-name (test-case-invoking-suite (last-case)))
		       'stack-consistency-tests)
	 (run 'stack-object-tests)
	 (ensure-equal (coerce-to-class-name (test-case-invoking-suite (last-case)))
		       'stack-object-tests)
	 (setq stack-suite (last-suite))
	 (ensure-type stack-suite stack-object-tests))
  (:documentation "Tests whether we can execute the tests in the stack test
  suite."))

(defsuite suite-definition-tests (framework-tests)
  ()
  (:setup (when (test-suite-p 'stack-object-tests) (undefine-stack-test-suite))
	  (when (test-suite-p 'stack-consistency-tests) (undefine-stack-consistency-suite)))
  (:teardown (when (test-suite-p 'stack-object-tests) (undefine-stack-test-suite))
	     (when (test-suite-p 'stack-consistency-tests) (undefine-stack-consistency-suite)))
  ;; This is a precondition for the suite.
  (:test ()
	 "Make sure that stack-object-tests is not a test suite."
	 (ensure (not (test-suite-p 'stack-object-tests)))
	 (ensure (not (test-suite-p 'stack-consistency-tests))))
  (:test ()
	 "Make sure stack-object-tests gets undefined correctly."
	 (define-stack-test-suite)
	 (ensure (test-suite-p 'stack-object-tests))
	 (ensure-compare (length (get-suite-cases 'stack-object-tests)) 1)
	 (define-stack-consistency-suite)
	 (ensure (test-suite-p 'stack-consistency-tests))
	 (ensure-compare (length (get-suite-cases 'stack-consistency-tests)) 3)
	 (undefine-stack-consistency-suite)
	 (ensure (not (test-suite-p 'stack-consistency-tests)))
	 (undefine-stack-test-suite)
	 (ensure (not (test-suite-p 'stack-object-tests))))
  (:documentation "Tests whether we can define test suites correctly."))

(defcase (suite-definition-tests)
  "Check if the test cases in a test are numbered in a stable way."
  (define-stack-test-suite)
  (let ((test-cases-1 (get-suite-cases 'stack-object-tests)))
    (undefine-stack-test-suite)
    (define-stack-test-suite)
    (let ((test-cases-2 (get-suite-cases 'stack-object-tests)))
      (ensure (and (null (set-difference test-cases-1 test-cases-2))
		   (= (length test-cases-1) (length test-cases-2))))))
  (undefine-stack-test-suite))

(defsuite suite-io-tests (framework-tests)
  ()
  (:setup (define-test-storage :root *framework-tests-dir*)
	  ;; Delete first to make sure you aren't reading a bad storage file.
	  ;; This can happen say if the delete-on-undefine is broken.
	  (when (test-object-p 'stack-object-tests)
	    (let ((stack-test-file (compute-test-object-storage-path 'stack-object-tests)))
	      (remove-test-storage 'stack-object-tests)
	      (when (probe-file stack-test-file)
		(delete-file stack-test-file))
	      (undefine-stack-test-suite)))
	  (define-stack-test-suite))
  (:teardown (undefine-stack-test-suite)
	     (storage-commit 'stack-object-tests)
	     (let ((stack-test-file (compute-test-object-storage-path 'stack-object-tests)))
	       (when (probe-file stack-test-file)
		 (delete-file stack-test-file))))
  (:documentation "Tests whether test suite files can be written out and
  read into FReT."))

(defcase (suite-io-tests)
  "Test for repeatable writes of a test suite."
  (ensure (object-modified-p 'stack-object-tests *default-test-storage*))
  (let ((string1 (with-output-to-string (str)
		   (write-test 'stack-object-tests :stream str))))
    (ensure (object-modified-p 'stack-object-tests *default-test-storage*))
    (write-test 'stack-object-tests)
    (undefine-stack-test-suite)
    (load-test 'stack-object-tests)
    (let ((string2 (with-output-to-string (str)
		     (write-test 'stack-object-tests :stream str))))
      ;; An approximation -- make sure the write does not produce an empty string
      (ensure (> (length string1) 50))
      (ensure-equal string1 string2 :test #'string=))))

(defcase (suite-io-tests io-mod-test-0)
  "Test for ensuring the initial state does not have a storage object."
  (ensure (null (get-object-storage 'stack-object-tests)))
  (ensure (null (get-object-storage 'stack-always-list-test))))

(defcase (suite-io-tests io-mod-test-1)
  "Test for modification wrt test storage."
  (ensure (object-modified-p 'stack-object-tests *default-test-storage*))
  (ensure (object-modified-p 'stack-always-list-test *default-test-storage*)))

(defcase (suite-io-tests io-mod-test-2)
  "Test for modification wrt test storage."
  (setf (get-object-storage 'stack-object-tests) *default-test-storage*)
  (ensure (object-modified-p 'stack-object-tests))
  (ensure (object-modified-p 'stack-always-list-test)))

(defcase (suite-io-tests io-mod-test-3)
  "Test for modification wrt test storage."
  (setf (get-object-storage 'stack-object-tests) *default-test-storage*)
  (ensure (object-modified-p 'stack-object-tests))
  (ensure (object-modified-p 'stack-always-list-test))
  (storage-commit 'stack-object-tests)
  (ensure (not (object-modified-p 'stack-object-tests)))
  (ensure (not (object-modified-p 'stack-always-list-test))))

(defcase (suite-io-tests io-mod-test-4)
  "Test for modification wrt test storage."
  (setf (get-object-storage 'stack-object-tests) *default-test-storage*)
  (ensure (object-modified-p 'stack-object-tests))
  (ensure (object-modified-p 'stack-always-list-test))
  (storage-commit 'stack-object-tests)
  (ensure (not (object-modified-p 'stack-object-tests)))
  (ensure (not (object-modified-p 'stack-always-list-test)))
  (undefine-stack-test-suite)
  (ensure (object-modified-p 'stack-object-tests))
  (ensure (object-modified-p 'stack-always-list-test)))

(defcase (suite-io-tests io-mod-test-5)
  "Test for modification wrt test storage."
  (setf (get-object-storage 'stack-object-tests) *default-test-storage*)
  (ensure (object-modified-p 'stack-object-tests))
  (ensure (object-modified-p 'stack-always-list-test))
  (storage-commit 'stack-object-tests)
  (ensure (not (object-modified-p 'stack-object-tests)))
  (ensure (not (object-modified-p 'stack-always-list-test)))
  (undefine-stack-test-suite)
  (ensure (object-modified-p 'stack-object-tests))
  (ensure (object-modified-p 'stack-always-list-test))
  (load-test 'stack-object-tests)
  (ensure (not (object-modified-p 'stack-object-tests)))
  (ensure (not (object-modified-p 'stack-always-list-test))))

(defsuite suite-modify-tests (framework-tests)
  ()
  (:setup (define-stack-test-suite))
  (:teardown (when (test-suite-p 'stack-object-tests)
	       (undefine-stack-test-suite)))
  (:documentation "Tests whether suite properties are correctly modified
  through various accessors."))

(defcase (suite-modify-tests the-suite-modify-test-case)
    "Test suite renaming."
  (flet ((read-exps-from-string (string)
	   (with-input-from-string (stream string)
	     (loop with eof = stream
		 for exp = (read stream nil eof)
		 until (eq exp eof)
		 collect exp))))
    (let ((original (read-exps-from-string
		     (with-output-to-string (str)
		       (write-test 'stack-object-tests :stream str :recursive t))))
	  (renamed nil)
	  (done-p nil))
      (unwind-protect
	  (progn
	    (rename-suite 'stack-object-tests 'new-suite-name)
	    (setq renamed
	      (subst 'stack-object-tests 'new-suite-name
		     (read-exps-from-string
		      (with-output-to-string (str)
			(write-test 'new-suite-name :stream str :recursive t)))))
	    (ensure (and (= (length original) (length renamed))
			 (null (set-difference original renamed :test #'equal))))
	    (setq done-p t))
	(when done-p
	  (undefsuite new-suite-name))))))

(defscript fret-test-script
  :setup nil
  :teardown nil
  :steps (suite-definition-tests
	  suite-execution-tests
	  suite-io-tests
	  (suite-modify-tests the-suite-modify-test-case)))

;;; EOF
