;;; -*- Mode: Lisp -*-

;;; This program is open source.  For license terms, see the LICENSE file.

;;; $Id: scoli.asd 132 2009-02-26 05:33:56Z smishra $

(defsystem scoli
  :name "SCOLI"
  :author "Sunil Mishra <smishra@sfmishras.com>"
  :licence "AFL v3"
  :description "A Common Lisp interface to the REST API of sesame v2."
  :components ((:file "package")
	       (:file "ntriple" :depends-on ("package"))
	       (:file "sesame" :depends-on ("package" "ntriple")))
  :depends-on ("drakma" "cxml" "cl-rdfxml"))

;;; EOF
