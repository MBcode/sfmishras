;;; -*- Mode: Lisp -*-

#||
Copyright (c) 2006, Sunil Mishra
All rights reserved.
||#

;;; $Id$

(eval-when (:load-toplevel :execute)
  (setf (logical-pathname-translations "haxl")
	`(("**;*.*.*" ,(merge-pathnames (make-pathname :directory '(:relative :wild-inferiors)
						       :name :wild :type :wild)
					*load-pathname*)))))

(asdf:defsystem haxl
  :pathname "haxl:"
  :components ((:file "package")
	       (:file "queue" :depends-on ("package"))
	       (:file "defs" :depends-on ("queue"))
	       (:file "xpath" :depends-on ("package"))
	       (:file "xml" :depends-on ("package" "defs" "xpath")))
  :depends-on ("cxml" "mcpat" "cl-ppcre"))

;;; EOF
