;;; -*- Mode: Lisp; Package: HAXL -*-

#||
Copyright (c) 2006-2007, Sunil Mishra
All rights reserved.
||#

;;; $Id$

(in-package "HAXL")

(defvar *developer-mode* t)

;;;----------------------------------------
;;; Conditions

(define-condition haxl-error (error)
  ())

(define-condition cannot-modify-xml (haxl-error)
  ((action :initarg :action)
   (object :initarg :object)
   (element :initarg :element))
  (:report (lambda (c stream)
	     (format stream "Cannot perform ~S of ~S on ~S."
		     (slot-value c 'action)
		     (slot-value c 'object)
		     (slot-value c 'element)))))

(define-condition document-not-found (haxl-error)
  ((name :initarg :name))
  (:report (lambda (c stream)
	     (format stream "Can't find XML template named ~S." (slot-value c 'name)))))

;;;----------------------------------------
;;; DTD Management

(defconstant +dtd-path+ #p"haxl:dtds;")

(defun default-entity-resolver (public-id system-id)
  (declare (ignore public-id))
  (when (eq (puri:uri-scheme system-id) :http)
    (let* ((uri-path (puri:uri-path system-id))
	   (dtd-filename (subseq uri-path (1+ (position #\/ uri-path :test #'char= :from-end t))))
	   (dtd-path (merge-pathnames (pathname dtd-filename) +dtd-path+)))
      (open dtd-path :element-type '(unsigned-byte 8)))))

;;;----------------------------------------
;;; Document management

(defclass xml-document ()
  ((name :initarg :name :reader xml-document-name)
   (path :initarg :path :accessor xml-document-path)
   (content :initarg :content :accessor xml-document-content)
   (read-time :initform (get-universal-time) :accessor xml-document-read-time)))

(defparameter *document-path-print-limit* 20
  "Parameter to control how much of the path of an xml-document instance is
  printed in the print-object method.")

(defmethod print-object ((obj xml-document) stream)
  "Print an xml-document instance. The instance is printed with its type,
  its name, and its path prefix."
  (print-unreadable-object (obj stream)
    (let ((path-string (namestring (xml-document-path obj))))
      (when (> (length path-string) *document-path-print-limit*)
	(let ((prefix-size (- *document-path-print-limit* 3)))
	  (setq path-string
		(with-output-to-string (str)
		  (write-string path-string str :start 0 :end prefix-size)
		  (write-string "..." str)))))
      (format stream "~A: ~A (~A)"
	      (symbol-name (type-of obj))
	      (xml-document-name obj)
	      path-string))))

(defmethod xml-document-content :around ((doc xml-document))
  (with-slots (path read-time) doc
    (cond ((not *developer-mode*) (call-next-method))
	  ((not (probe-file path)) nil)
	  ((> (file-write-date path) read-time)
	   (reread-xml-document doc)
	   (call-next-method))
	  (t (call-next-method)))))

(defmethod (setf xml-document-content) :after (content (doc xml-document))
  (declare (ignore content))
  (setf (xml-document-read-time doc) (get-universal-time)))

(defun register-document-directory (dir configuration)
  (pushnew dir (configuration-directories configuration) :test #'equal)
  dir)

(defun reread-xml-document (doc)
  (setf (xml-document-content doc) (document-xml-content (xml-document-path doc)))
  doc)

(defun check-new-document (name configuration)
  "Given a name and a configuration, find an XML file with that name. We
  only look for a new document in *developer-mode*."
  (when *developer-mode*
    (do-configuration-hierarchy (conf configuration)
      (dolist (dir (configuration-directories conf))
	(let* ((path (make-pathname :name name :type "xml" :defaults dir))
	       (content (when (probe-file path)
			  (read-a-document path conf))))
	  (when content
	    (return-from check-new-document content)))))))

(defun find-xml-document (name configuration &optional (check-new-p t))
  "Find an XML document in a configuration."
  (or (do-configuration-hierarchy (conf configuration result)
	(setq result (find name (configuration-documents conf)
			   :key #'xml-document-name :test #'string=)))
      (when check-new-p
	(check-new-document name configuration))))

(defmacro do-xml-document ((g-name g-document
			    &optional (configuration '*root-configuration*) g-result)
			   &body body)
  "Iterate through all XML documents in a configuration, executing body on
   each one. The inner loop is a named loop, to allow a return in body to
   exit the outer loop."
  (let ((g-configuration (gensym "configuration"))
	(g-document-record (gensym "document-record"))
	(g-conf (gensym "conf"))
	(g-inner (gensym "inner")))
    `(let ((,g-configuration ,configuration))
       (do-configuration-hierarchy (,g-conf ,g-configuration ,g-result)
	 (loop named ,g-inner
	    for ,g-document-record in (configuration-documents ,g-conf)
	    for ,g-name = (xml-document-name ,g-document-record)
	    for ,g-document = (xml-document-content ,g-document-record)
	    do (progn ,@body))))))

(defun find-document-xml (name &optional (error-p t) (configuration *root-configuration*))
  (let ((document (find-xml-document name configuration)))
    (cond (document (xml-document-content document))
	  (error-p (error 'document-not-found :name name))
	  (t nil))))

(defun update-xml-document (document-path xml configuration)
  (let* ((name (pathname-name document-path))
	 (old-document (find-xml-document name configuration nil)))
    (when old-document
      (unless (equal document-path (xml-document-path old-document))
	(warn 'xml-document-path-change :name name
	      :new-path document-path :old-path (xml-document-path old-document))
	(setf (xml-document-path old-document) document-path))
      (setf (xml-document-content old-document) xml)
      old-document)))

(defun add-xml-document (document-path xml configuration)
  (let* ((name (pathname-name document-path))
	 (new-document (make-instance 'xml-document
				      :name name :path document-path :content xml)))
    (push new-document (configuration-documents configuration))
    new-document))

(defun document-xml-content (document-path)
  (cxml:parse-file document-path
		   #+allegro (rune-dom:make-dom-builder)
		   #+openmcl (cxml-dom:make-dom-builder)
		   :entity-resolver #'default-entity-resolver))

(defun copy-document-xml (name &optional (error-p t) (configuration *root-configuration*))
  "Retrieve and make a copy of the XML document identified by name. If
  error-p is true, signal an error if the document is not found. Otherwise
  return nil. configuration is the configuration instance where we shall
  search for the document."
  (let ((xml (find-document-xml name error-p configuration)))
    ;; error-p will have been handled in find-document-xml
    (when xml
      (dom:clone-node xml t))))

(defun read-a-document (document-path configuration)
  (let ((xml (document-xml-content document-path)))
    (or (update-xml-document document-path xml configuration)
	(add-xml-document document-path xml configuration))))

(defun read-document-directory (dir &optional (configuration *root-configuration*))
  (register-document-directory dir configuration)
  (dolist (document-path (directory (make-pathname :name :wild :type "xml" :defaults dir)))
    (read-a-document document-path configuration)))

;;;----------------------------------------
;;; XML Node Manipulation

(defun remove-element-id (element)
  (when (dom:get-attribute-node element "id")
    (dom:remove-attribute element "id")))

(defun remove-element (element)
  (dom:remove-child (dom:parent-node element) element))

(defun prepare-for-copying (element)
  (remove-element-id element)
  (multiple-value-prog1 (values (dom:parent-node element) (dom:next-sibling element))
    (remove-element element)))

(defun create-xhtml-element (document element)
  (dom:create-element-ns document *xhtml-ns-string* element))

(defun set-or-insert-text (element content)
  (let ((children (dom:child-nodes element)))
    (cond ((= (length children) 0)
	   (dom:insert-before element (dom:create-text-node (dom:owner-document element) content)
			      nil))
	  ((or (> (length children) 1) (not (dom:text-node-p (aref children 0))))
	   (error 'cannot-modify-xml :action :insert :object content :element element))
	  (t (setf (dom:data (aref children 0)) content)))))

;;;----------------------------------------
;;; XML Node Set Manipulation

(defun set-value (attrs value)
  (dolist (attr attrs)
    (setf (dom:value attr) value)))

(defun set-text (texts value)
  (dolist (text texts)
    (setf (dom:data text) value)))

(defun insert-in (nodes content reference)
  (loop for node in nodes
      for document = (dom:owner-document node)
      for imported-content = (dom:import-node document content t)
      do (dom:insert-before node imported-content
			    (case reference
			      (last nil)
			      (t (error "Not implemented."))))))

;;;----------------------------------------
;;; Template Based Manipulation

(defvar *current-template-node*)
(defvar *current-node*)
(defvar *current-parent*)
(defvar *succeeding-node*)

(defun current-node ()
  *current-node*)

(defun xml-node-selector (node class element id)
  "Produce an expression suitable for applying to an XML node, based on the
  selection parameter. Only one of these is assumed to be an expression,
  the rest are assumed to be NIL. The expression is used as a search
  parameter relative to *current-node*. For node, we return the input
  node, we return the input. For class, we search the descendents of
  *current-node* for an element node with the input as its class attribute
  value. id is similar to class. For element, with search the descendent
  nodes with the given element name."
  (cond (node node)
	(class `(car (access-path *current-node* (descendent * [ (attribute "class") = ,class ]))))
	(id `(car (access-path *current-node* (descendent * [ (attribute "id") = ,id ]))))
	(element `(car (access-path *current-node* (descendent ,element))))))

(defun set-attribute (attribute value)
  (multiple-value-bind (ns local-name)
      (expand-qname attribute)
    (if ns
	(dom:set-attribute-ns *current-node* ns local-name value)
	(dom:set-attribute *current-node* attribute value))))

(defun set-style (attribute value)
  (let ((style-value (dom:get-attribute *current-node* "style")))
    (setq style-value
	  (if style-value
	      (if (search attribute style-value)
		  (cl-ppcre:regex-replace (format nil "(^|;)(\\s*)~A:[^;]*" attribute)
					  style-value
					  (format nil "\\1\\~A: ~A" attribute value))
		  (format nil "~A: ~A; ~A" attribute value style-value))
	      (format nil "~A: ~A" attribute value)))
    (dom:set-attribute *current-node* "style" style-value)))

(defun copy-template ()
  (dom:insert-before *current-parent*
		     (setq *current-node* (dom:clone-node *current-template-node* t))
		     *succeeding-node*))

(defun fill-node (data &optional (*current-node* *current-node*))
  "Remove all the child nodes of the *current-node*. Then, insert new text
  nodes containing data. This function thus treats *current-node* as a
  template to be filled with the supplied data."
  (dom:do-node-list (child (dom:child-nodes *current-node*))
    (dom:remove-child *current-node* child))
  (dom:insert-before *current-node*
		     (dom:create-text-node (dom:owner-document *current-node*)
					   (cond ((stringp data) data)
						 ((symbolp data) (symbol-name data))
						 (t (princ-to-string data))))
		     nil))

(defun replace-with-imported-element (replacement &optional (*current-node* *current-node*))
  "Replace *current-node* with its replacement. The replacement is assumed
  to be from a different document. We first import the replacement into the
  document of the element, then replace the *current-node* with the import.
  The result is the imported replacement element."
  (let ((new-element (dom:import-node (dom:owner-document *current-node*) replacement t)))
    (dom:replace-child (dom:parent-node *current-node*) new-element *current-node*)
    new-element))

(defmacro with-xml-template ((&key node class element id) &body body)
  "Copy a selected element for use as a template. This macro helps manage
  the replication of a section of an XML document multiple times in the
  result. *current-template-node* is bound to the node being used as a
  template. *current-node* is set whenever a copy-template is executed,
  which the input body is responsible for executing. See xml-node-selector
  for the use of node, class, element and id."
  (let ((g-xml-node (xml-node-selector node class element id)))
    `(let* ((*current-template-node* ,g-xml-node)
	    (*current-node* nil))
       (multiple-value-bind (*current-parent* *succeeding-node*)
	   (prepare-for-copying *current-template-node*)
	 ,@body))))

(defmacro with-xml ((&key node class element id) &body body)
  "This macro selects an XML element for further manipulation, binding it
  to *current-node*. The given body is responsible for manipulating the
  selected node. See xml-node-selector for the use of node, class, element
  and id."
  (let ((g-xml-node (xml-node-selector node class element id)))
    `(let* ((*current-node* ,g-xml-node)
	    (*current-parent* (dom:parent-node *current-node*)))
       ,@body)))

;;;----------------------------------------
;;; Output

(defun write-xml (doc &optional (stream *standard-output*))
  (dom:map-document
   (cxml:make-namespace-normalizer
    #+allegro (cxml:make-character-stream-sink stream :canonical nil)
    #+openmcl (cxml:make-character-stream-sink/utf8 stream :canonical nil))
   doc :include-doctype :canonical-notations))

(defun write-redirect-xml (uri &optional (configuration *root-configuration*)
			   (stream *standard-output*))
  "Constructs an XML document for redirecting the user to another
  document. We first locate a suitable XML template, and fill the href
  that will point to the redirect target. We finally write out the document
  to stream."
  (let* ((redirect-template (copy-document-xml "redirect" t configuration))
	 (aref (dom:get-element-by-id redirect-template "redirect-target")))
    (dom:set-attribute aref "href" uri)
    (fill-node aref uri)
    (write-xml redirect-template stream)))

(defun write-form-document-xml (form-name
				&optional (configuration *root-configuration*)
				          (stream *standard-output*)
				&rest form-values)
  (let* ((form-template (copy-document-xml form-name t configuration))
	 (form (aref (dom:get-elements-by-tag-name-ns form-template *xhtml-ns-string* "form") 0))
	 (form-inputs (access-path form (descendent "xhtml:input" [ attribute "name" ])))
	 (inputs-table (loop for input in form-inputs
			     for name = (dom:get-attribute input "name")
			     when (and (stringp name) (> (length name) 0))
			     collect (cons name input))))
    (loop for key in form-values by #'cddr
	  for value in (cdr form-values) by #'cddr
	  for element = (cdr (assoc key inputs-table :test #'string=))
	  do (dom:set-attribute element "value" value))
    (write-xml form-template stream)))

;;;----------------------------------------
;;; Initialization

(defun initialize-haxl ()
  (read-document-directory #p"haxl:xml;"))

;;; EOF
