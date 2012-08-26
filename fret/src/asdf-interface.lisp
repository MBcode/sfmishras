;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: FRET; Base: 10 -*-

#|
Copyright (c) 2004,2007,2008 Sunil Mishra
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

;;; Here's how I want the ASDF integration to work... When I type in
;;;         (asdf:operate 'fret:fret-op 'fret-test)
;;; asdf should test my system. In general, we should be able to execute
;;; the test operation without loading the software first. However, since
;;; the fret software defines classes and packages essential for the test
;;; system to make sense, so we must make an exception for FReT itself. The
;;; tests should proceed a component at a time, starting with the top level
;;; and proceeding down to the leaf components. Any component that wants
;;; tests executed should define storage, a test script, and optionally a
;;; system for test support code. First, the test support code will be
;;; loaded. Then, starting with the leafmost component defining a test,
;;; test scripts will be executed.

(defvar *default-component-test-storage* (make-pathname :directory '(:relative "test"))
  "The default location to look relative to an .asd file for test files.")

#|
(defun compute-asdf-system-test-root ()
  (declare (special *asdf-system*))
  (make-pathname :name nil :type nil
		 :defaults (merge-pathnames #p"test/" (asdf:component-pathname *asdf-system*))))
|#

(defclass component (asdf:component)
  ((test-storage :initarg :test-storage :initform *default-component-test-storage*
		 :accessor component-test-storage
		 :documentation "The storage mechanism for test cases.")
   (test-name :initarg :test-name :accessor component-test-name
	      :documentation "The default test script for the component."))
  (:documentation "The basic FReT ASDF component."))

(defclass module (asdf:module component)
  ()
  (:documentation "The basic testable module."))

(defclass system (asdf:system module)
  ()
  (:documentation "The testable system."))

(defun setup-component-test-storage (obj)
  "Instantiates test storage for the component obj."
  (with-slots (test-storage) obj
    (etypecase test-storage
      ((or string pathname)
       (let ((storage-path (merge-pathnames (pathname test-storage) (asdf:component-pathname obj))))
	 (setf test-storage
	       (make-instance 'file-system-test-storage :root storage-path))))
      (file-system-test-storage))))

(defmethod shared-initialize :after ((obj component) slot-names &rest args &key &allow-other-keys)
  "Initializes a component. The only task performed here is to set up
  component storage, only when the name slot of the component is bound."
  (declare (ignore slot-names args))
  (when (and (slot-boundp obj 'asdf::name)
	     (slot-boundp obj 'asdf::relative-pathname))
    (setup-component-test-storage obj)))

(defclass fret-op (asdf:load-op)
  ((tested :initform nil :accessor tested
	   :documentation "The components that have been tested in the course of this operation.")
   (break-on-errors :initarg :break-on-errors :initform t :accessor break-on-errors
		    :documentation "Whether FReT should break on errors found by the test suite.")
   (force-reload :initarg :force-reload :initform nil :accessor force-reload
		 :documentation "If we should force the test object to reload."))
  (:documentation "The FReT testing operation."))

(defmethod asdf:operation-done-p ((op fret-op) (component component))
  "Always returns NIL, ensuring perform will be executed on the given
  component."
  nil)

(defmethod asdf:perform :around ((op fret-op) (component component))
  "Ensure the component hasn't already been performed on. And add the
  component to the executed operations if it hasn't been performed."
  (unless (member component (tested op))
    (push component (tested op))
    (call-next-method)))

(defmethod asdf:perform ((op fret-op) (component component))
  "Execute the given fret-op on component, which involves loading the test
  from storage and executing the test named in the component. Accepts test
  parameters provided in the operation."
  (let ((storage (component-test-storage component))
	(test-name (component-test-name component)))
    (dolist (d (asdf:component-depends-on (make-instance 'asdf:compile-op) component))
      (dolist (system (cdr d))
	(setq system (asdf:find-system system))
	(when (typep system 'component)
	  (asdf:perform op system))))
    (load-test test-name :storage storage :force (force-reload op) :recursive t)
    (run test-name :break-on-errors (break-on-errors op))))

(defun test-fret ()
  "Execute the FReT test suite via ASDF."
  (let ((*default-pathname-defaults* (asdf:component-pathname (asdf:find-system 'fret))))
    (asdf:operate 'fret-op 'fret-test)))

;;; EOF
