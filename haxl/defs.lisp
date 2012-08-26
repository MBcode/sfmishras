;;; -*- Mode: Lisp; Package: HAXL -*-

#||
Copyright (c) 2007, Sunil Mishra
All rights reserved.
||#

;;; $Id$

(in-package "HAXL")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *haxl-directory*
    (make-pathname :name nil :type nil
		   :defaults (or *compile-file-pathname* *load-pathname*))
    "Path to the HAXL root directory."))

(defclass configuration ()
  ((name :initarg :name :reader configuration-name)
   (directories :initarg :directories :initform nil :accessor configuration-directories)
   (documents :initarg :documents :initform nil :accessor configuration-documents)
   (parents :initarg :parents :initform nil :accessor configuration-parents))
  (:documentation "A configuration is a collection of directories and their
  contained XML documents. The configuration stores and manages documents
  for an application employing HAXL. A configuration also has parents, a
  facility that can serve to organize applications in more powerful ways."))

(defmethod print-object ((obj configuration) stream)
  "Print method that prints the name of the configuration unreadably."
  (print-unreadable-object (obj stream)
    (format stream "~A: ~A" (symbol-name (type-of obj)) (configuration-name obj))))

(defmacro do-configuration-hierarchy ((g-iterator configuration &optional g-result) &body body)
  "A convenience macro to iterate through all configurations and ancestor
  configurations."
  (let ((g-q (gensym "queue"))
	(g-success-p (gensym "success-p")))
    `(let ((,g-q (make-queue (list ,configuration)))
	   ,@(when g-result (list g-result)))
       (loop
	  (multiple-value-bind (,g-iterator ,g-success-p)
	      (queue-dequeue ,g-q)
	    (when (or (not ,g-success-p) ,g-result)
	       (return ,g-result))
	    ,@body
	    (dolist (parent (configuration-parents ,g-iterator))
	      (queue-enqueue ,g-q parent)))))))

(defvar *root-configuration*
  (make-instance 'configuration
		 :name 'haxl-root-configuration
		 :directories (list (merge-pathnames (make-pathname :directory '(:relative "xml"))
						     *haxl-directory*)))
  "The root of all configurations. This configuration acts as a fallback,
  giving applications access to default documents if the application has
  not provided a substitute.")

(defun make-configuration (name &rest parents)
  "Creates a new configuration. If no parents are specified for the
  configuration, the *root-configuration* is used as a default."
  (make-instance 'configuration :name name :parents (or parents (list *root-configuration*))))

(defun add-configuration-directory (configuration directory)
  "Add the directory to the list of configuration directories."
  (pushnew directory (configuration-directories configuration) :test #'equal))

;;; EOF
