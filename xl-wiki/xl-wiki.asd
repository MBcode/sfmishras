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

(defpackage #:xl-wiki.system
  (:use #:cl
        #:asdf))

(in-package #:xl-wiki.system)

(defsystem #:xl-wiki
    :version "0.3.1"
    :author "Stefan Scholl <stesch@no-spoon.de>"
    :licence "Lesser Lisp General Public License"
    :depends-on (#:hunchentoot #:cl-emb #:cl-ppcre #:cl-who)
    :components ((:file "packages")
                 (:file "util")
                 (:file "colorize-package")
                 (:file "encode-for-pre")
                 (:file "coloring-css" :depends-on ("colorize-package"))
                 (:file "colorize" :depends-on ("colorize-package"
                                                "coloring-css"
                                                "encode-for-pre"))
                 (:file "coloring-types" :depends-on ("colorize"))
		 (:file "defs" :depends-on ("packages"))
                 (:file "codes" :depends-on ("defs" "colorize" "coloring-types"))
                 (:file "wiki" :depends-on ("packages" "defs" "util" "codes"))))

;;; EOF
