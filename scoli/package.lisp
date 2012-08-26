;;; -*- Mode: Lisp; Package: COMMON-LISP-USER; Syntax: Ansi-Common-Lisp; Base: 10 -*-

;;; This program is open source.  For license terms, see the LICENSE file.

;;; $Id: package.lisp 138 2009-03-26 18:22:10Z smishra $

(in-package "COMMON-LISP-USER")

(defpackage "SCOLI"
  (:use "COMMON-LISP")
  (:export "SESAME-CONNECTION"		; Classes
	   "CONNECT-SESAME"		; API
	   "GET-PROTOCOL"
	   "GET-REPOSITORIES"
	   "REPOSITORY-QUERY"
	   "REPOSITORY-RETRIEVE"
	   "REPOSITORY-STORE"))

(defpackage "NTRIPLE"
  (:use "COMMON-LISP")
  (:export "SERIALIZE"
	   "STRINGIFY"))

;;; EOF
