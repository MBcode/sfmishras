;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: COMMON-LISP-USER; Base: 10 -*-

;;; This software is Copyright (c) Sunil Mishra, 2008.
;;; Sunil Mishra grants you the rights to distribute
;;; and use this software as governed by the terms
;;; of the Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html),
;;; known as the LLGPL.

;;; $Id$

(defsystem cl-wise
  :version "0.0"
  :author "Sunil Mishra <smishra@sfmishras.com>"
  :license "Lesser Lisp General Public License"
  :depends-on (cl-wiki)
  :components ((:file "package")
	       (:file "wiki-extns" :depends-on ("package"))))

;;; EOF
