;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: COMMON-LISP-USER; Base: 10 -*-

;;; This software is Copyright (c) Sunil Mishra, 2008.
;;; Sunil Mishra grants you the rights to distribute
;;; and use this software as governed by the terms
;;; of the Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html),
;;; known as the LLGPL.

;;; $Id$

(in-package "COMMON-LISP-USER")

(defpackage "CL-WISE"
  (:nicknames "WISE")
  (:use "COMMON-LISP" "CL-WIKI")
  (:shadow "START")
  (:export "START"))

;;; EOF
