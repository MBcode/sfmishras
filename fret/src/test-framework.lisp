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

;;;;----------------------------------------
;;;; General constants

(defvar +suite-var+ (gensym (symbol-name '#:suite))
  "A constant gensymed once, used repeatedly, for test suite objects.")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar +ensure-body-var+ (gensym (symbol-name '#:ensure-body))
    "A constant gensymed once, used repeatedly, for representing the
body of ensure statements."))

(declaim (special *current-test*))

;;;;----------------------------------------
;;;; Ensure failure condition

(define-condition failed-ensure (error)
  ((test-case :reader failed-test-case :initarg :test-case :initform *current-test*)
   (body :reader failure-body :initarg :body :initform (error-required 'body 'failed-ensure)))
  (:report (lambda (c stream)
	     (format stream "Test case ~A failed on ~S." (failed-test-case c) (failure-body c))))
  (:documentation "The condition type thrown when ensures fail."))

(define-condition slot-required (error)
  ((slot :reader required-slot :initarg :slot)
   (class :reader required-in-class :initarg :class))
  (:report (lambda (c stream)
	     (format stream "Class ~A requires slot ~A to be given."
		     (required-in-class c) (required-slot c))))
  (:documentation "Condition thrown when a class is not initialized with all required arguments."))

(defun error-required (slot class)
  (cerror "Continue with value nil." 'slot-required :class class :slot slot)
  nil)

;;;;----------------------------------------
;;;; Managing history

(defvar *test-stack* nil)

(defun coerce-to-class (obj)
  (cond ((typep obj 'standard-class) obj)
	((symbolp obj) (find-class obj))
	((typep obj 'standard-object) (class-of obj))
	(t (error "Cannot coerce ~S to a class." obj))))

(defun coerce-to-class-name (obj)
  (cond ((symbolp obj) obj)
	((typep obj 'standard-class) (class-name obj))
	((typep obj 'standard-object) (class-name (class-of obj)))
	(t (error "Cannot coerce ~S to a class name." obj))))

(defun unuse-test (obj)
  ;; OBJ could either be a class name or a test object instance
  (setq *test-stack* (delete (coerce-to-class-name obj) *test-stack* :key #'coerce-to-class-name)))

(defun last-test ()
  (car *test-stack*))

(defun use-test (obj)
  "Place the given test suite to the front of the list of test suites."
  (unuse-test obj)
  (push obj *test-stack*))

(defun rotate-forward (l)
  (let ((i (pop l)))
    (nconc l (list i)))
  l)

(defun rotate-backward (l)
  (cond ((null (cdr l)))
	((null (cddr l)) (rotatef (car l) (cadr l)))
	(t (let ((end (last l 2)))
	     (push (cadr end) l)
	     (setf (cdr end) nil))))
  l)

(defun next-test ()
  (setq *test-stack* (rotate-forward *test-stack*))
  (last-test))

(defun previous-test ()
  (setq *test-stack* (rotate-backward *test-stack*))
  (last-test))

(defun next-matching-test (predicate)
  (let ((rot-tests (member-if predicate *test-stack*)))
    (when rot-tests
      (setq *test-stack* (nconc rot-tests (ldiff *test-stack* rot-tests)))
      (last-test))))

(defun previous-matching-test (predicate)
  (let ((rot-tests
	 (loop for rest-stack on *test-stack*
	       for match-stack = nil then (if (funcall predicate (car rest-stack))
					      rest-stack
					    match-stack)
	       finally (return match-stack))))
    (when rot-tests
      (setq *test-stack* (nconc rot-tests (ldiff *test-stack* rot-tests)))
      (last-test))))

;;;;----------------------------------------
;;;; CLOS operations

(defun find-disconnected-class-subtree (class root-class)
  (unless (subtypep class root-class)
    (let ((result nil))
      (labels ((traverse-subtree (cl)
		 (push cl result)
		 (dolist (scl (mop:class-direct-subclasses cl))
		   (unless (or (subtypep scl root-class)
			       (member scl result))
		     (traverse-subtree scl)))))
	(traverse-subtree class)
	(nreverse result)))))

(defun get-class-precedence-list (class)
  "Return the class precedence list for the given class. For some CL
  implementations we must also ensure the class is finalized."
  #+(or allegro sbcl)
  (unless (mop:class-finalized-p class)
    (mop:finalize-inheritance class))
  (mop:class-precedence-list class))

;;;;----------------------------------------
;;;; Ensure predicates

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro defensure (name (&rest args) &body body)
    "A macro is useful for writing the user level ENSURE macros. Common
features of the ensure macros, such as error handling and restarts, would
be implemented here."
    (let (doc-string)
      (when (stringp (car body))
	(setq doc-string (pop body)))
      `(defmacro ,name (&whole ,+ensure-body-var+ ,@args)
	 ,doc-string
	 `(or (progn ,,@body)
	      (error 'failed-ensure :body ',,+ensure-body-var+))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defensure ensure (&body body)
    "The basic ensure macro that succeeds if the given body evaluates to
a non-nil value."
    `(progn ,@body)))

(defensure ensure-condition (&rest body)
  "The usage of this macro is:
    (ensure-condition [condition-type] <body>)
Tests if body throws a condition of type condition-type, otherwise fails."
  (let ((type 'error))
    (when (subtypep (car body) 'condition)
      (setq type (pop body)))
    `(block nil
       (handler-bind ((,type (lambda (c)
			       (declare (ignore c))
			       (return t))))
	 ,@body
	 nil))))

(defmacro ensure-warning (&body body)
  "Tests if BODY throws a warning. This macro is implemented using
ENSURE-CONDITION."
  `(ensure-condition warning ,@body))

(defmacro ensure-error (&body body)
  "Tests if BODY throws an error. This macro is implemented using
ENSURE-CONDITION."  
  `(ensure-condition error ,@body))

(defensure ensure-compare (eval-form &rest result-forms-and-options)
  "The usage of this macro is:
    (ensure-compare <form> <value>* &key test strict-multiple-value-p)
<form> is evaluated, and the (potentially multiple) result values are
compared to the provided list of values <value>*. TEST is used to compare
the return values against the expected values. If STRICT-MULTIPLE-VALUE-P
is provided, the return values of <form> should exactly match <value>*.
Otherwise, it is only required that each value in <value>* have a
corresponding return value from <form>."
  ;; Remove all the options from the tail of the results list
  (let ((test #'equal)
	(strict-multiple-value-p nil)
	(result-forms nil))
    (loop for options-first on (reverse result-forms-and-options) by #'cddr
	for opt = (cadr options-first)
	for val = (car options-first)
	while (or (eq opt :test) (eq opt :strict-multiple-value-p))
	if (eq opt :test)
	do (setq test val)
	else do (setq strict-multiple-value-p val)
	finally (setq result-forms (reverse options-first)))
    ;; Write the comparison form
    (let ((mv-test-p (cdr result-forms)))
      `(let ((eval-out ,(if mv-test-p
			    `(multiple-value-list ,eval-form)
			  eval-form))
	     (results-out ,(if mv-test-p
			       `(list ,@result-forms)
			     (car result-forms))))
	 (and ,@(when (and mv-test-p strict-multiple-value-p)
		  `((= (length eval-out) (length results-out))))
	      ,(if mv-test-p
		   `(every ,test eval-out results-out)
		 `(funcall ,test eval-out results-out)))))))

(defmacro ensure-equal (eval-form &rest result-forms-and-options)
  "An application of ENSURE-COMPARE with TEST set to EQUAL."
  `(ensure-compare ,eval-form ,@result-forms-and-options))

(defensure ensure-type (eval-form type)
  "Ensures that the return value of EVAL-FORM is a subtype of TYPE."
  `(typep ,eval-form ',type))

(defun succeed ()
  "A function hard-wired to signal test success."
  (ensure t))

(defun fail () 
  "A function hard-wired to signal test failure."
  (ensure nil))

;;;;----------------------------------------
;;;; Dependency Recording

;;; Assumption: We will not have to record dependencies for more than one
;;;             object at any given time. Otherwise the queue structure
;;;             becomes more complex.

;;; Queue Data Structure
;;; The car of the first cons points to the push end of the queue.
;;; The cdr of the first cons points to the pop end of the queue.

(defun make-queue ()
  (let ((q (list nil)))
    (setf (car q) q)))

(defun queue-empty-p (q)
  (eq (car q) q))

(defun queue-singleton-p (q)
  (eq (car q) (cdr q)))

(defun queue-push (item q)
  (push item (cdr (car q)))
  (setf (car q) (cdr (car q))))

(defun queue-pop (q)
  (cond ((queue-empty-p q) nil)
	((queue-singleton-p q)
	 (prog1 (pop (cdr q))
	   (setf (car q) q)))
	(t (pop (cdr q)))))

;;; Dependency queue handling

(declaim (special *dependence-queue-id* *remaining-dependencies* *handled-dependencies*))

(defun execute-dependence-queue (queue-id executor initializer)
  (let ((*dependence-queue-id* queue-id)
	(*remaining-dependencies* (make-queue))
	(*handled-dependencies* nil))
    (funcall initializer)
    (loop with key and data
	  for record = (queue-pop *remaining-dependencies*)
	  while record
	  if (consp record)
	  do (setq key (car record) data (cdr record))
	  else do (setq key record data nil)
	  unless (member key *handled-dependencies*)
	  do (funcall executor key data)
	     (push key *handled-dependencies*))))

(defun record-dependence (key &rest data)
  (when (and (boundp '*dependence-queue-id*) *dependence-queue-id*)
    (unless (member key *handled-dependencies*)
      (queue-push (if data (cons key data) key) *remaining-dependencies*))))

;;;;----------------------------------------
;;;; Hooks

;;; Operations:
;;; - hook-add
;;; - hook-remove
;;; - hook-run

(defstruct hook
  name
  arguments
  table)

(defvar *hook-list* nil)
(defvar +arglist-keywords+ '(&optional &key &rest &aux &allow-other-keys))

(defun find-hook (name &optional (error-p t))
  (or (find name *hook-list* :key #'hook-name)
      (when error-p
	(error "No hook named ~S." name))))

(defun ensure-simple-arglist (args)
  ;; For now we want to only deal with simple argument lists
  ;; A more comprehensive one might do otherwise
  (assert (null (intersection +arglist-keywords+ args))))

(defun arglists-congruent-p (list1 list2)
  (= (length list1) (length list2)))

(defun create-hook (name args)
  (ensure-simple-arglist args)
  (let ((hook (find name *hook-list* :key #'hook-name)))
    (cond (hook
	   (setf (hook-arguments hook) args))
	  (t
	   (setq hook (make-hook :name name :arguments args))
	   (push hook *hook-list*)))
    hook))

#+allegro
(defun get-function-arguments (fn)
  "Get the argument list for the given function."
  (excl:arglist fn))

#+sbcl
(defun get-function-arguments (fn)
  "Get the argument list for the given function."
  (sb-introspect:function-arglist fn))

#+openmcl
(defun get-function-arguments (fn)
  "Get the argument list for the given function."
  (ccl:arglist fn))

(defun check-hook-function-arguments (fn hook-name)
  (let ((fn-args (get-function-arguments fn)))
    (ensure-simple-arglist fn-args)
    (assert (arglists-congruent-p fn-args (hook-arguments (find-hook hook-name))))))

(defun hook-add (hook-name entry-name entry-fn)
  (check-hook-function-arguments entry-fn hook-name)
  (let* ((hook (find-hook hook-name))
	 (entry (assoc entry-name (hook-table hook))))
    (if entry
	(setf (cdr entry) entry-fn)
	(push (cons entry entry-fn) (hook-table hook)))))

(defun hook-remove (entry-name hook-name)
  (let ((hook (find-hook hook-name)))
    (setf (hook-table hook)
	  (delete entry-name (hook-table hook) :key #'car))))

(defun hook-run (hook-name &rest args)
  (dolist (entry (hook-table (find-hook hook-name)))
    (apply (cdr entry) args)))

(defmacro define-hook (hook-name (&rest args) &rest init-hooks)
  `(progn
     (create-hook ',hook-name ',args)
     ,@(loop for (entry-name entry-fn) in init-hooks collect
	 `(hook-add ',hook-name ',entry-name ,entry-fn))
     ',hook-name))

#|
(fret:define-test-storage :root #p"c:/cygwin/home/smishra/lisp/fret/test/")
(fret:load-test 'fret::fret-test-script :force t)
(fret:run 'fret::fret-test-script :break-on-errors t)
|#

;;; EOF
