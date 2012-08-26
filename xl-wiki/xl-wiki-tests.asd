;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Base: 10 -*-

;;; This software is Copyright (c) Sunil Mishra, 2008.
;;; Stefan Scholl grants you the rights to distribute
;;; and use this software as governed by the terms
;;; of the Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html),
;;; known as the LLGPL.

;;; $Id$

(in-package #:cl-user)

(asdf:defsystem xl-wiki-tests
  :class fret:system
  :test-name xl-wiki::wiki-test-script
  :depends-on (fret))

;;; EOF
