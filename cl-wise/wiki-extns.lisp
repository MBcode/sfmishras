;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: COMMON-LISP-USER; Base: 10 -*-

;;; This software is Copyright (c) Sunil Mishra, 2008.
;;; Sunil Mishra grants you the rights to distribute
;;; and use this software as governed by the terms
;;; of the Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html),
;;; known as the LLGPL.

;;; $Id$

(in-package "CL-WISE")

(defun slot-required (slot class)
  "Basic utility function that signals an error that states the slot wasn't
  initialized with a value. Intended for use in the initform of a slot."
  (error "~A requires a value for ~A." class slot))

;;;----------------------------------------
;;; Topic and Segmentation

(defparameter *topic-start-scanner*
  (cl-ppcre:create-scanner "&lt;topic(.*?)&gt;" :multi-line-mode t)
  "Regular expression scanner that finds topic starts.")

(defparameter *topic-end-scanner*
  (cl-ppcre:create-scanner "&lt;/topic&gt;")
  "Regular expression scanner that finds topic ends.")

(defparameter *attr-val-scanner*
  (cl-ppcre:create-scanner "\\s*(\\w+?)\\s*=\\s*(['\"])(.+?)\\2\\s*")
  "Regular expression that detects attribute values.")

(defclass topic ()
  ((parent :initarg :parent :initform (slot-required 'parent 'topic) :reader topic-parent
	   :documentation "The parent topic.")
   (object :initarg :object :initform (slot-required 'object 'topic) :accessor topic-object
	   :documentation "The object the topic describes.")
   (relations :initarg :relations :initform nil :accessor topic-relations
	      :documentation "The relations of the topic to the parent."))
  (:documentation "Object to store and process topic tags inserted into a
  document. Segmentation produces a topic tree that provides the backbone
  for the semantic interpretation of the document."))

(defun segment-topic-attribute-values (topic-string)
  "Segment the attribute/value portion of the topic tag into a list of
  alternating attribute and value strings."
  (let ((result nil)
	(match-end 0)
	match-start regs-start regs-end)
    (loop
       (multiple-value-setq (match-start match-end regs-start regs-end)
	 (ppcre:scan *attr-val-scanner* topic-string :start match-end))
       (unless match-start
	 (return-from segment-topic-attribute-values (nreverse result)))
       (push (subseq topic-string (aref regs-start 0) (aref regs-end 0)) result)
       (push (subseq topic-string (aref regs-start 2) (aref regs-end 2)) result))))

(defun interpret-topic-tag-attribute-value (attr val)
  "Parse the given attribute and value into a set of keyword and value
  arguments that can be used for initializing a topic instance."
  (cond ((string-equal attr "target")
	 (let ((target-content (ppcre:split "::" val)))
	   (case (length target-content)
	     (1 (cons :object target-content))
	     (2 (list :relations (car target-content) :object (cadr target-content))))))))

(defun make-topic-node (topic-string parent)
  "Create a new topic node based on the topic tag's attribute values and
  the topic parent. If the attribute values cannot be reasonably made sense
  of, then we return nil."
  (apply #'make-instance 'topic
	 :parent parent
	 (or (loop with topic-segments = (segment-topic-attribute-values topic-string)
		for attr in topic-segments by #'cddr
		for val in (cdr topic-segments) by #'cddr
		append (interpret-topic-tag-attribute-value attr val))
	     (return-from make-topic-node nil))))

;;; Segmentation --------------------------

(defun scan-for-topic-start (string cursor)
  "Return the topic start position in the given string starting from the
  position indicated by the cursor. We return the position where the topic
  open is found, the attribute values for the topic open, and the end of
  the topic open. If no topic open is found, we return NIL."
  (multiple-value-bind (match-start match-end regs-start regs-end)
      (cl-ppcre:scan *topic-start-scanner* string :start cursor)
    (when match-start
      (values match-start (subseq string (aref regs-start 0) (aref regs-end 0)) match-end))))

(defun scan-for-topic-end (string cursor)
  "Return the topic end position in the given string starting from the
  point indicated by the cursor. We return the start and end of the topic
  close tag. NIL is returned if a topic close isn't found."
  (multiple-value-bind (match-start match-end)
      (cl-ppcre:scan *topic-end-scanner* string :start cursor)
    (values match-start match-end)))

(defun end-topic-segmentation (string cursor)
  "Called when there's no further segmentation in the document. Returns the
  remainder of the string."
  (when (< cursor (length string))
    (subseq string cursor)))

(defun segment-at-topic-end (string cursor topic-end)
  "Called when a topic end tag is encountered. Returns a list of the string
  to the end of the topic."
  (when (< cursor topic-end)
    (subseq string cursor topic-end)))

(defun segment-at-topic-start (string cursor topic-start)
  "Called when a topic start tag is encountered. Returns the string up to
  the start of the topic."
  (when (< cursor topic-start)
    (subseq string cursor topic-start)))

(defun segment-by-topics (string cursor parent)
  "Segment the given string by the topic tags within. Each topic open tag
  results in the creation of a new child node. Each topic close tag results
  in backtracking. We thus get a topic tree."
  ;; Find <topic> -- ot
  ;; Find </topic> -- ct
  ;; If ot is immediately followed by ct, that's a segment
  ;; If ot is followed by ot, then that's a segment
  ;; If ct is followed by ct, that's a segment
  (multiple-value-bind (topic-start topic past-topic-start)
      (scan-for-topic-start string cursor)
    (multiple-value-bind (topic-end past-topic-end)
	(scan-for-topic-end string cursor)
      (cond ((and (null topic-start) (null topic-end))
	     (end-topic-segmentation string cursor))
	    ((or (null topic-start) (< topic-end topic-start))
	     (values (segment-at-topic-end string cursor topic-end)
		     past-topic-end))
	    ((or (null topic-end) (< topic-start topic-end))
	     (let ((topic-node (make-topic-node topic parent)))
	       (multiple-value-bind (topic-segment new-cursor)
		   (segment-by-topics string past-topic-start topic-node)
		 (setq topic-segment
		       (cond ((and topic-node (consp topic-segment))
			      (cons topic-node topic-segment))
			     ((null topic-node)
			      topic-segment)
			     (t (list topic-node topic-segment))))
		 (multiple-value-bind (post-segment final-cursor)
		     (segment-by-topics string new-cursor parent)
		   (let ((pre-segment (segment-at-topic-start string cursor topic-start)))
		     (values
		      (if (> (count-if #'identity (list topic-segment pre-segment post-segment)) 1)
			  (nconc (if (listp pre-segment)
				     pre-segment
				     (list pre-segment))
				 (if (listp topic-segment)
				     topic-segment
				     (list topic-segment))
				 (if (listp post-segment)
				     post-segment
				     (list post-segment)))
			  (or pre-segment topic-segment post-segment))
		      final-cursor))))))
	    (t (error "Unexpected segmentation situation."))))))

;;;----------------------------------------
;;; Wiki Specialization

(defclass wise-wiki (wiki)
  ()
  (:documentation "Wiki class that implements a semantic wiki."))

(defclass wise-context ()
  ((assertions :initarg nil :accessor wise-assertions))
  (:documentation "The interpretation context for a wiki page. The context
  collects all the assertions encountered during interpretation."))

(defmethod make-translate-context ((server wise-wiki))
  "Create a translation context when interpreting a page. The context will
  gather all the semantic assertions contained in a page."
  (make-instance 'wise-context))

(defmethod segment ((server wise-wiki) string)
  "Segmentation of input string by topic."
  (segment-by-topics string 0 nil))

(defmethod post-escape-rewrites ((server wise-wiki))
  "Rewrite patterns for the semantic wiki. The wiki adds additional
  interpretations for annotated links."
  (let* ((rewrites (call-next-method))
	 (link-points (member :link-with-alternate-text rewrites :key #'car))
	 (prelink-rewrites (ldiff rewrites link-points))
	 (semlink-points
	  `((:semantic-link-with-text
	     "\\[\\[(.*)::([^]\">|]*?)\\|([^]\">]*?)\\]\\]"
	     ("<a href=\"" ,(make-rep-url-encoder 1) "\" title=\"" 1 "\">" 2 "</a>"))
	    (:semantic-link
	     "\\[\\[(.*)::(.*?)\\]\\]"
	     ("<a href=\"" ,(make-rep-url-encoder 1) "\" title=\"" 1 "\">" 1 "</a>")))))
    (append prelink-rewrites semlink-points link-points)))

;;;----------------------------------------
;;; Main

(defun start ()
  "Startup function for the semantic wiki."
  (wiki:start 'wise-wiki))

;;; EOF
