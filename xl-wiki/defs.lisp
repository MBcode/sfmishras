;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: XL-WIKI; Base: 10 -*-

;;; This software is Copyright (c) Stefan Scholl, 2005-2008.
;;; Stefan Scholl grants you the rights to distribute
;;; and use this software as governed by the terms
;;; of the Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html),
;;; known as the LLGPL.

;;; Portions Copyright (c) Sunil Mishra, 2008.

;;; $Id$

(in-package :xl-wiki)

(defvar *wiki-port* 5757
  "Port XL-WIKI listens for http connections.")

(defvar *wiki-directory* nil
  "Base directory for data.")

(defvar *wiki-template-directory*
  (merge-pathnames "templates/"
                   (asdf:component-pathname (asdf:find-system :xl-wiki)))
  "Base directory for templates.")

(defvar *wiki-page-list* '((:title "Home" :link "Home")
                           (:title "About" :link "About")
                           (:title "Contact" :link "Contact"))
  "Plist of pages displayed on every page. :LINK can have parameters.")

(defvar *wiki-home-page* "Home"
  "Default page when root directory of the site is accessed.")

(defvar *behind-proxy-p* nil
  "Indicates if XL-WIKI runs behind a reverse proxy.")

(defvar *page-index* (make-hash-table :test #'equal)
  "Global index over all wiki pages.")

(defvar *edit-textarea-size* '(:rows 25 :cols 80)
  "List of two integer values containing number of rows and columns of textarea
of edit form.")

;; -------------------------------------------------------------------

(defvar *wiki-server* nil
  "The current wiki server instance.")

(defvar *emb-lock* (tbnl-mp:make-lock "emb-lock")
  "Lock for CL-EMB.")

(defvar *page-index-lock* (tbnl-mp:make-lock "page-index-lock")
  "Lock for *PAGE-INDEX*")

(eval-when (:load-toplevel)
  (defvar *wiki-software-directory*
    (make-pathname :name nil :type nil :defaults *load-pathname*)
    "The location of the wiki software. This is used as the default for the
    configuration file and the wiki data directory."))

;; -------------------------------------------------------------------

(defclass wiki ()
  ((http-server :initarg :http-server :initform nil :accessor http-server))
  (:documentation "The base wiki class."))

;;; EOF
