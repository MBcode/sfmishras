;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: FRET; Base: 10 -*-

#|
Copyright (c) 2004,2009, Sunil Mishra
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
;;;; Output to File

(defvar *default-test-storage* nil
  "The object that controls how test objects are output to a storage medium.
The default test storage medium is the file system, and the default test
storage strategy is to create a file for each test object in a test storage
directory. See also *DEFAULT-TEST-STORAGE-CLASS*, DEFINE-TEST-STORAGE,
DEFAULT-OBJECT-MAPPING, and FILE-SYSTEM-TEST-STORAGE.")
(defvar *default-test-storage-class* 'file-system-test-storage
  "The default class for describing test storage strategies.")
(defvar *default-test-file-type* "lisp"
  "The default type used when reading or writing test files to storage.
This is used in the context of FILE-SYSTEM-TEST-STORAGE.")
(defvar *default-object-mapping* 'default-object-mapping
  "The default object mapping function that maps a test object to a file
system object. This is used in the context of FILE-SYSTEM-TEST-STORAGE.")
(defvar *test-object-storage* (make-hash-table))
(defvar *test-modification-times* (make-hash-table))
(defvar *write-truename* nil)

;;;;----------------------------------------
;;;; Storage definition

(defclass test-storage ()
  ()
  (:documentation "The abstract test storage class."))

(defclass persistent-test-storage (test-storage)
  ((commit-times :initform (make-hash-table) :reader storage-test-commit-times))
  (:documentation "The abstract class to represent persistent stores."))

(defclass stream-test-storage (test-storage)
  ((stream :initarg :stream :initform nil :accessor test-storage-stream))
  (:documentation "A class that specifies the storage destination to be a
stream. This is useful if the output requires no additional decoration, as
is the case for output to an interactive stream such as *STANDARD-OUTPUT*."))

(defclass file-system-test-storage (stream-test-storage persistent-test-storage)
  ((root :initarg :root :initform (error-required 'root 'file-system-test-storage)
	 :reader test-storage-root)
   (object-mapping :initarg :object-mapping :initform *default-object-mapping*
		  :reader test-object-mapping)
   (file-type :initarg :file-type :initform *default-test-file-type*
	      :reader test-storage-file-type))
  (:documentation "A class that organizes test object definitions in file
system objects. ROOT is the root location where the tests reside. This
might be, for example, the software's root, or a test directory in under
software's root. OBJECT-MAPPING holds a function that maps the test object
to a particular file. It should take two arguments, the storage object
and the test object name. FILE-TYPE stores the type used for test object
files. STREAM is used to store the file stream opened while there is a
test object being written."))

(defun compute-test-object-storage-path (object-name &optional (storage *default-test-storage*))
  (funcall (test-object-mapping storage) storage object-name))

(defmethod test-storage-root :around ((storage file-system-test-storage))
  (let ((root (call-next-method)))
    (if (functionp root)
	(funcall root)
      root)))

(defun default-object-mapping (storage object-name)
  "The default mapping from test object to a test file. The entire object
is mapped to a file of the same name, lowercased, in the root directory of
the storage object. The storage object's type is used as the file type."
  (let ((root (test-storage-root storage))
	(object-name (if (test-case-p object-name)
			 (test-case-defining-suite object-name)
			 object-name)))
    (make-pathname :name (string-downcase (symbol-name object-name))
		   :type (test-storage-file-type storage) :defaults root)))

(defun make-test-storage (class args)
  (apply #'make-instance class args))

(defun define-test-storage (&rest args &key (class *default-test-storage-class*) &allow-other-keys)
  "This function is used for instantiating storage objects. It takes a
:CLASS keyword argument that defaults to *DEFAULT-TEST-STORAGE-CLASS*.
A storage class instance of :CLASS is instantiated using the rest of
the arguments as initargs. *DEFAULT-TEST-STORAGE* is set to this instance."
  (remf args :class)
  (setq *default-test-storage* (make-test-storage class args)))

;;; Storage tracking

(defun get-object-storage (object)
  (gethash (coerce-to-class-name object) *test-object-storage*))

(defun (setf get-object-storage) (value object)
  (assert (typep value 'persistent-test-storage))
  (setf (gethash (coerce-to-class-name object) *test-object-storage*) value))

(defun remove-object-storage (object storage &optional (time (get-universal-time)))
  (let* ((name (coerce-to-class-name object)))
    (remhash name (storage-test-commit-times storage))
    (remhash name *test-object-storage*)
    (setf (gethash name *test-modification-times*) time)))

;;; Modification tracking

(defun object-modified-time (object)
  (gethash (coerce-to-class-name object) *test-modification-times*))

(defun (setf object-modified-time) (value object)
  (setf (gethash (coerce-to-class-name object) *test-modification-times*) value))

(defun object-commit-time (object &optional storage)
  (let* ((name (coerce-to-class-name object))
	 (storage (or storage (get-object-storage name) *default-test-storage*)))
    (when storage
      (gethash name (storage-test-commit-times storage)))))

(defun (setf object-commit-time) (value object &optional storage)
  (let* ((name (coerce-to-class-name object))
	 (storage (or storage (get-object-storage name) *default-test-storage*)))
    (when storage
      (setf (gethash name (storage-test-commit-times storage)) value))))

(defun object-modified-p (object &optional storage)
  (let* ((name (coerce-to-class-name object))
	 (storage (or storage (get-object-storage name) *default-test-storage*)))
    (when storage
      (let ((mod-time (object-modified-time name))
	    (commit-time (object-commit-time name storage)))
	(cond ((null mod-time)
	       (assert (test-object-p name) ())
	       (error "Test object ~S doesn't have a modification time." name))
	      ((null commit-time) t)
	      (t (< commit-time mod-time)))))))

;;;;----------------------------------------
;;;; Writing

(defgeneric write-test-to-storage (case-obj storage force)
  (:method ((case-class symbol) storage force)
    (write-test-to-storage (find-class case-class) storage force))
  (:method ((case-class standard-class) storage force)
    (write-test-to-storage (get-test-object-prototype case-class) storage force)))

;;; Writing a case

(defmethod write-test-to-storage :around ((ti test-case) (storage file-system-test-storage) force)
  #-sbcl (declare (ignore force))
  (let* ((suite-name (test-case-defining-suite ti))
	 (*package* (symbol-package suite-name)))
    (call-next-method)))

(defmethod write-test-to-storage :around ((ti test-case) (storage stream-test-storage) force)
  #-sbcl (declare (ignore force))
  (let ((*print-case* :downcase)
	(*print-pretty* t))
    (call-next-method)))

(defmethod write-test-to-storage ((ti test-case) (storage stream-test-storage) force)
  #-sbcl (declare (ignore force))
  (let* ((stream (test-storage-stream storage))
	 (case-class (class-of ti))
	 (case-name (class-name case-class))
	 (suite-name (test-case-defining-suite ti)))
    (format stream "~2%(~S (~S ~S)" 'defcase suite-name case-name)
    (format stream "~@[~&    ~S~]" (documentation case-class t))
    (format stream "~{~&~2T~S~})" (test-case-body ti))))

;;; Writing a suite

(defun write-suite-test-case-definitions (si storage force)
  (dolist (test-class (get-suite-cases si))
    (write-test-to-storage test-class storage force)))

(defmethod write-test-to-storage :around 
	   ((si test-suite) (storage file-system-test-storage) force)
  (let* ((suite-name (class-name (class-of si)))
	 (file (compute-test-object-storage-path suite-name storage))
	 (*package* (symbol-package suite-name))
	 (*write-truename* file))
    (when (or force (object-modified-p suite-name storage))
      (with-open-file
	  (stream file :direction :output :if-exists :supersede :if-does-not-exist :create)
	(setf (test-storage-stream storage) stream)
	(unwind-protect
	    (progn
	      (format stream "~&;;; -*- Mode: Lisp -*-")
	      (format stream "~2%;;; Generated test suite definitions for ~S" suite-name)
	      (format stream "~2%(in-package ~S)" (make-symbol (package-name *package*)))
	      (call-next-method))
	  (setf (test-storage-stream storage) nil))
	(format stream "~2%;;; EOF")))))

(defmethod write-test-to-storage :around ((si test-suite) (storage stream-test-storage) force)
  (let ((*print-case* :downcase)
	(*print-pretty* t))
    (call-next-method)
    (write-suite-test-case-definitions si storage force)))

(defmethod write-test-to-storage ((si test-suite) (storage stream-test-storage) force)
  #-sbcl (declare (ignore force))
  (let* ((stream (test-storage-stream storage))
	 (suite-class (class-of si))
	 (suite-name (class-name suite-class))
	 (supers (mapcar #'class-name (mop:class-direct-superclasses suite-class))))
    (when (eq (car (last supers)) 'test-suite)
      (setq supers (butlast supers)))
    (format stream "~2%(~S ~S ~S" 'defsuite suite-name supers)
    (format stream "~&  ~S" (get-suite-slot-definitions si))
    (format stream "~{~&~2T~S~}" (get-suite-class-options si))
    (let ((setup-body (get-suite-setup si)))
      (when setup-body
	(format stream "~&  (:setup ~{~10,0:T~S~^~%~})" setup-body)))
    (let ((teardown-body (get-suite-teardown si)))
      (when teardown-body
	(format stream "~&  (:teardown ~{~13,0:T~S~^~%~})" teardown-body)))
    (format stream "~&  ;; Do not modify this manually, especially to a smaller value.")
    (format stream "~&  (:case-sequence-start ~D))" (get-case-sequence suite-name))))

;;; Writing a script

(defmethod write-test-to-storage :around
    ((si test-script) (storage file-system-test-storage) force)
  "If we're writing the test script to file system storage, write out the
  file decoration and create an output stream. Then call the next method."
  (let* ((script-name (class-name (class-of si)))
	 (file (compute-test-object-storage-path script-name storage))
	 (*package* (symbol-package script-name))
	 (*write-truename* file))
    (when (or force (object-modified-p script-name storage))
      (with-open-file
	  (stream file :direction :output :if-exists :supersede :if-does-not-exist :create)
	(setf (test-storage-stream storage) stream)
	(unwind-protect
	     (progn
	       (format stream "~&;;; -*- Mode: Lisp -*-")
	       (format stream "~2%;;; Generated test script definition for ~S" script-name)
	       (format stream "~2%(in-package ~S)" (make-symbol (package-name *package*)))
	       (call-next-method))
	  (setf (test-storage-stream storage) nil))
	(format stream "~2%;;; EOF")))))

(defmethod write-test-to-storage :around ((si test-script) (storage stream-test-storage) force)
  "If we're writing out to stream storage, we just bind a few output
  related variables to useful values, and call the next method."
  (declare (ignore force))
  (let ((*print-case* :downcase)
	(*print-pretty* t))
    (call-next-method)))

(defmethod write-test-to-storage ((si test-script) (storage stream-test-storage) force)
  "The primary method for writing a test script to stream storage. This
  method produces the actual output of the test script."
  #-sbcl (declare (ignore force))
  (let* ((stream (test-storage-stream storage))
	 (script-class (class-of si))
	 (script-name (class-name script-class)))
    (format stream "~2%(~S ~S" 'defscript script-name)
    (let ((setup-body (test-script-setup-body si)))
      (when setup-body
	(format stream "~&  :setup ~S" setup-body)))
    (let ((teardown-body (test-script-teardown-body si)))
      (when teardown-body
	(format stream "~&  :teardown ~S" teardown-body)))
    (let ((steps (test-script-steps si)))
      (format stream "~&  :steps ~{~9,0:T~S~^~%~})" steps))))

(defmethod write-test-to-storage :after
    ((tobj test-object) (storage file-system-test-storage) force)
  (declare (ignore force))
  (setf (get-object-storage tobj) storage)
  (update-test-modified tobj storage))

(defmethod write-test-to-storage ((test-class standard-class) storage force)
  (write-test-to-storage (get-test-object-prototype test-class) storage force))

(defun write-test-subtree (name storage force)
  (cond ((test-suite-p name)
	 (dolist (test-class (get-class-precedence-list (find-class name)))
	   (when (eq (class-name test-class) 'test-suite)
	     (return))
	   (write-test-to-storage (class-name test-class) storage force)))
	((test-script-p name)
	 (write-test-to-storage name storage force)
	 (dolist (step (test-script-steps (get-test-object-prototype (find-class name))))
	   (etypecase step
	     (cons (write-test-subtree (car step) storage force))
	     (symbol (write-test-subtree step storage force)))))
	(t (error "Invalid input ~S." name))))

(defun write-test (name &key (recursive t) force stream storage)
  (let* ((stream (if (eq stream t) *standard-output* stream))
	 (*default-test-storage*
	  (cond (stream
		 (assert (null storage) () "Cannot specify both STORAGE and STREAM.")
		 (make-test-storage 'stream-test-storage (list :stream stream)))
		(t (or storage (get-object-storage name) *default-test-storage*)))))
    (cond ((test-case-p name) (write-test-to-storage name *default-test-storage* force))
	  ((or (test-suite-p name) (test-script-p name))
	   (if recursive
	       (write-test-subtree name *default-test-storage* force)
	       (write-test-to-storage name *default-test-storage* force)))
	  (t (error "Don't know how to write ~S." name)))))

;;;;----------------------------------------
;;;; Loading

(defgeneric load-test-from-storage (test storage force)
  )

;;; Loading a suite

(defmethod load-test-from-storage ((test-class standard-class) storage force)
  ;; Possible redefinition
  (let ((si (get-test-object-prototype test-class)))
    (load-test-from-storage si storage force)))

(defmethod load-test-from-storage ((si test-object) storage force)
  (if force
      (load-test-from-storage (class-name (class-of si)) storage force)
    (warn "~A cannot test for modifications to its storage file. Assuming not modified." 
	  (class-name (class-of si)))))

(defmethod load-test-from-storage ((si test-case) (storage file-system-test-storage) force)
  (declare (ignore force))
  (warn "Default file system storage does not store individual cases. Load the suite instead."))

(defmethod load-test-from-storage ((si standard-object) storage force)
  (load-test-from-storage (class-name (class-of si)) storage force))

(defmethod load-test-from-storage ((object-name symbol) (storage file-system-test-storage) force)
  (declare (ignore force))		; We'd only be here if we wanted to load
  (let ((old-defns (when (test-object-p object-name)
		     (with-output-to-string (str)
		       (write-test object-name :stream str :recursive nil))))
	done-p)
    ;; An error loading the object should reliably undefine the object
    (unwind-protect
	(progn
	  (load (compute-test-object-storage-path object-name storage)
		:verbose (check-verbosity-threshold :fine))
	  (setq done-p t))
      (when (not done-p)
	(cond (old-defns
	       (let ((defn-start 0)
		     (eof old-defns)
		     defn)
		 (loop
		  (multiple-value-setq (defn defn-start)
		    (read-from-string old-defns nil eof :start defn-start))
		  (when (eq defn eof)
		    (return))
		  (eval defn))))
	      ((test-case-p object-name) (eval `(undefcase ',object-name)))
	      ((test-suite-p object-name) (eval `(undefsuite ',object-name)))
	      ((test-script-p object-name) (eval `(undefscript ',object-name))))))))

(defmethod load-test-from-storage :after ((object-name symbol) storage force)
  (declare (ignore force))
  (setf (get-object-storage object-name) storage))

(defmethod load-test-from-storage :around ((object test-object) (storage file-system-test-storage)
					   force)
  "When loading a test object from file system storage, we must ensure that
  the last modification time for the object was before the start of the
  load opertion. Otherwise, the likely situation is that the definition of
  this test object is stored in the same file as the object that caused the
  load, and therefore an explicit load would be undesirable."
  (declare (special *load-start-time*)
	   (ignore force))
  (when (or (typep object 'test-case)
	    (let ((mod-time (object-modified-time object)))
	      (or (null mod-time) (< mod-time *load-start-time*))))
    (call-next-method)))

(defun load-test-recursive (test-name storage force)
  (labels ((record-test-dependence (name)
	     (cond ((test-suite-p name)
		    (dolist (supersuite (mop:class-direct-superclasses (find-class name)))
		      (record-dependence (class-name supersuite) 'test-suite)))
		   ((test-script-p name)
		    (record-test-script-dependencies name))
		   (t (error "Expected ~S to be either suite or script." name))))
	   (test-loader (name)
	     (or (load-test-from-storage (or (find-class name nil) name)
					 storage force)
		 (record-test-dependence name)))
	   (recursive-loader (key data)
	     (declare (ignore data))
	     (let ((recorded-name key))
	       (unless (or (eq recorded-name 'test-suite) (eq recorded-name 'test-script))
		 (test-loader recorded-name)))))
    (execute-dependence-queue test-name
			      #'recursive-loader
			      (lambda ()
				(load-test-from-storage test-name storage force)))))

(defun load-test-nonrecursive (test-name storage force)
  (labels ((test-loader (name)
	     (load-test-from-storage (or (find-class name nil) name) storage force))
	   (test-defined-p (name type)
	     (ecase type
	       (test-suite (test-suite-p name))
	       (test-script (test-script-p name))
	       ((t) (or (test-suite-p name) (test-script-p name)))))
	   (nonrecursive-loader (key data)
	     (let ((recorded-name key)
		   (recorded-type (or (car data) t)))
	       (unless (test-defined-p recorded-name recorded-type)
		 (test-loader recorded-name)))))
    (execute-dependence-queue test-name
			      #'nonrecursive-loader
			      (lambda ()
				(load-test-from-storage test-name storage force)
				(setq force nil)))))

(defun load-test (name &key force recursive storage)
  "Read the given test object from STORAGE. A supplied argument is given
  preference. Otherwise, the previous storage is used, and failing that,
  *DEFAULT-TEST-STORAGE* is accessed. Unless FORCE is true, the test object
  is loaded only if it is out of date. It is up to the storage object to
  determine the test object's status. If RECURSIVE is true, we attempt to
  load everything on which this object depends, refreshing all those that
  are found to be out of date. If neither is supplied, only dependencies
  that are found to be undefined are loaded from storage. Before we begin
  any loading, we also bind *load-start-time* so that we can filter out
  unnecessary recursive loading."
  (let ((*default-test-storage*
	 (or storage
	     (when (test-object-p name)
	       (get-object-storage name))
	     *default-test-storage*))
	(*load-start-time* (get-universal-time)))
    (declare (special *load-start-time*))
    (if recursive
	(load-test-recursive name *default-test-storage* force)
	(load-test-nonrecursive name *default-test-storage* force))))

;;;;----------------------------------------
;;;; Deleting

(defmethod delete-test-from-storage ((object standard-class) (storage file-system-test-storage))
  (delete-test-from-storage (get-test-object-prototype object) storage))

(defmethod delete-test-from-storage ((test deleted-test-object) (storage file-system-test-storage))
  (delete-test-from-storage (class-name (class-of test)) storage))

(defmethod delete-test-from-storage ((test-name symbol) (storage file-system-test-storage))
  (let ((file (compute-test-object-storage-path test-name storage)))
    (when (and file (probe-file file))
      (delete-file file))))

;;;;----------------------------------------
;;;; Storage removal

;;; A method to call to delete the the association between an object
;;; and the given storage.

(defgeneric %remove-test-storage (test storage time)
  (:method ((test deleted-test-object) storage time)
    (remove-object-storage test storage time))
  (:method ((test test-object) storage time)
    (remove-object-storage test storage time)))

(defmethod %remove-test-storage ((si test-suite) (storage file-system-test-storage) time)
  (call-next-method)
  (dolist (test-case (get-suite-direct-cases si))
    (%remove-test-storage (get-test-object-prototype test-case) storage time)))

(defun collect-default-stores (name)
  (let ((stores nil)
	(recorded-storage (gethash name *test-object-storage*)))
    (when recorded-storage
      (setq stores (list recorded-storage)))
    (when *default-test-storage*
      (pushnew *default-test-storage* stores))
    stores))

(defun remove-test-storage (test &optional (time (get-universal-time)) &rest stores)
  (unless stores
    (setq stores (collect-default-stores (coerce-to-class-name test))))
  (dolist (storage stores)
    (%remove-test-storage (get-test-object-prototype test) storage time)))

;;;;----------------------------------------
;;;; Tracking modification

(defgeneric update-test-modified (test storage)
  (:method :around (test storage)	; Just an optimization
    (declare (ignore test))
    (when storage
      (call-next-method)))
  (:method (test storage)
    (declare (ignore storage test))
    nil)
  (:method ((test standard-class) storage)
    (update-test-modified (get-test-object-prototype test) storage)))

(defmethod update-test-modified :after ((ci test-case) (storage file-system-test-storage))
  (update-test-modified (find-class (test-case-defining-suite ci)) storage))

(defmethod update-test-modified ((tobj test-object) (storage file-system-test-storage))
  "The primary method for updating the last modification time of a test
  object stored in a file system. If we're neither reading nor writing to a
  file, or if we're writing, we use the current time. Otherwise if we're
  loading we use the file's modification time, only if the file is the same
  as the computed storage of the object. Otherwise the current time is used
  as the modification time. The reason for this is that we don't have a
  canonical storage-based representation of the test object, which means
  the update should be considered as not occurring through the storage
  object."
  (cond ((and (null *load-truename*) (null *write-truename*))
	 (setf (object-modified-time tobj) (get-universal-time)))
	((null *write-truename*)
	 (let ((obj-path (compute-test-object-storage-path (coerce-to-class-name tobj) storage)))
	   (if (equal *load-truename* obj-path)
	       (let ((time (file-write-date *load-truename*)))
		 (setf (object-modified-time tobj) time
		       (object-commit-time tobj storage) time))
	       (setf (object-modified-time tobj) (get-universal-time)))))
	((null *load-truename*)
	 (setf (object-commit-time tobj) (get-universal-time)))
	(t (error "Cannot be loading and writing ~S simultaneously!" tobj))))

;;;;----------------------------------------
;;;; User Interfaces

(defun storage-commit (&optional (test-name (coerce-to-class-name (last-test))) storage)
  (unless storage
    (setq storage (get-object-storage storage)))
  (unless storage
    (setq storage *default-test-storage*))
  (when (and storage (object-modified-p test-name storage))
    (if (subtypep test-name 'deleted-test-object)
	(delete-test-from-storage test-name storage)
	(write-test test-name :storage storage))))

(defun storage-commit-all (&optional storage)
  (declare (ignore storage))
  )

(defun storage-revert (&optional (test-name (coerce-to-class-name (last-test))))
  (declare (ignore test-name))
  )

(defun storage-revert-all ()
  )

(defun storage-status (&optional (storage *default-test-storage*))
  (declare (ignore storage))
  )

;;;;----------------------------------------
;;;; Hook Callbacks

;;; Compare load path with storage path and decide modified flag.

(defun test-storage-hook-listener (test-name)
  "Listener that acts when a persisted test is defined or deleted."
  (unless (get-object-storage test-name)
    (setf (get-object-storage test-name) *default-test-storage*))
  (update-test-modified (find-class test-name)  (get-object-storage test-name)))

(hook-add 'test-definition-hook 'storage #'test-storage-hook-listener)
(hook-add 'test-deletion-hook 'storage #'test-storage-hook-listener)

;;; EOF
