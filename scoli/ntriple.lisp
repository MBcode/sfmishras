;;; -*- Mode: Lisp; Package: SCOLI; Syntax: Ansi-Common-Lisp; Base: 10 -*-

;;; This program is open source.  For license terms, see the LICENSE file.

;;; $Id: ntriple.lisp 142 2009-03-27 17:39:52Z smishra $

(in-package "NTRIPLE")

(defgeneric serialize (obj stream)
  (:documentation "Serialize the given object into an ntriple string to
  the given stream.")
  (:method ((obj puri:uri) stream)
    (write-char #\< stream)
    (puri:render-uri obj stream)
    (write-char #\> stream)))

(defun stringify (obj)
  "Convert the input object into an ntriple string."
  (if (stringp obj)
      obj
      (with-standard-io-syntax
	  (with-output-to-string (str)
	    (serialize obj str)))))

;;; EOF
