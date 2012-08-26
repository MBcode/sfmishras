;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: FRET; Base: 10 -*-

#|
Copyright (c) 2007, Sunil Mishra
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

;;; $Id: framework-tests.lisp 68 2004-11-22 02:44:12Z smishra $

(in-package #:fret)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *framework-tests-dir*
    (merge-pathnames (make-pathname :name :unspecific :type :unspecific
				    :directory '(:relative :back "test"))
		     (or *load-truename* *compile-file-truename*))
    "The directory where the tests are located."))

;;; Support functions

(defun define-stack-test-suite ()
  "Creates a new stack-object-tests suite. This suite tests the basic
  representation of stacks."
  (defsuite stack-object-tests ()
    ((stack :initform nil))
    ;; Let's test a stack
    (:setup (setq stack nil))
    (:test (stack-always-list-test) (listp stack))))

(defun define-stack-consistency-suite ()
  "Creates a new stack-consistency-tests suite, which tests the basic
  push operation of the stack, and the state of the stack after the
  operation."
  (defsuite stack-consistency-tests (stack-object-tests)
    ()
    (:setup (push 'something stack))
    (:teardown (pop stack))
    (:test () (eq (car stack) 'something))
    (:test () (= (length stack) 1))))

(defun undefine-stack-test-suite ()
  "Removes the suite from persistent storage, and its definition."
  (remove-test-storage 'stack-object-tests)
  (undefsuite stack-object-tests))

(defun undefine-stack-consistency-suite ()
  "Removes the suite from persistent storage, and its definition."
  (remove-test-storage 'stack-consistency-tests)
  (undefsuite stack-consistency-tests))

;;; EOF
