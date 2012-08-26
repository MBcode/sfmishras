;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

;;; This software is Copyright (c) Stefan Scholl, 2005-2008.
;;; Stefan Scholl grants you the rights to distribute
;;; and use this software as governed by the terms
;;; of the Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html),
;;; known as the LLGPL.

;;; Portions Copyright (c) Sunil Mishra, 2008.

;;; $Id$

(in-package #:cl-user)

(defpackage #:xl-wiki
  (:nicknames #:wiki)
  (:use #:cl #:cl-who)
  (:export #:start
           #:stop
	   ;; Extension API
	   #:make-rep-url-encoder
	   #:make-translate-context
	   #:reconstitute
	   #:rewrite
	   #:post-escape-rewrites
	   #:pre-escape-rewrites
	   #:segment
	   #:translate
	   #:wiki
	   ))

;;; EOF
