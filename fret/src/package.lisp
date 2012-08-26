;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: COMMON-LISP-USER; Base: 10 -*-

#|
Copyright (c) 2004,2007, Sunil Mishra
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.
    * The names of its contributors may not be used to endorse or promote
      products derived from this software without specific prior written
      permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.
|#

;;; $Id$

(in-package #:common-lisp-user)

(defpackage #:fret
  (:use :common-lisp)
  (:export
   #:*current-test*
   #:*default-suite-mapping*
   #:*default-test-file-type*
   #:*default-test-storage*
   #:*default-test-storage-class*
   #:*test-break-on-errors*
   #:*test-output*
   #:*test-print-length*
   #:*test-print-level*
   #:*test-verbosity*
   #:add-suite-supertype
   #:component
   #:copy-suite
   #:defcase
   #:define-test-storage
   #:defscript
   #:defsuite
   #:delete-suite-test-cases
   #:ensure
   #:ensure-compare
   #:ensure-condition
   #:ensure-equal
   #:ensure-error
   #:ensure-type
   #:ensure-warning
   #:fail
   #:failed-ensure
   #:file-system-test-storage
   #:fret-op
   #:load-test
   #:message
   #:module
   #:print-case-result
   #:print-suite-result
   #:remove-suite-supertype
   #:rename-suite
   #:run
   #:set-suite-setup
   #:set-suite-teardown
   #:subst-suite-supertype
   #:succeed
   #:suite-file-modified-p
   #:system
   #:test-case
   #:test-fret
   #:test-script
   #:test-storage
   #:test-suite
   #:undefcase
   #:undefsuite
   #:use-case
   #:use-suite
   #:write-test
   ))

;;; SBCL should add a nickname mop to the package sb-pcl.
#+sbcl
(let ((pkg (find-package :sb-pcl))
      (mop (find-package :mop)))
  (unless (or (null mop) (eq mop pkg))
    (error "Package MOP already defined to point to something other than sb-pcl."))
  (unless mop
    (sb-ext:with-unlocked-packages (:sb-pcl)
      (rename-package pkg (package-name pkg)
		      `(,@(package-nicknames pkg) ,(symbol-name '#:mop))))))

#+openmcl
(let ((pkg (find-package :ccl))
      (mop (find-package :mop)))
  (unless (or (null mop) (eq mop pkg))
    (error "Package MOP already defined to point to something other than CCL."))
  (unless mop
    (rename-package pkg (package-name pkg)
		    `(,@(package-nicknames pkg) ,(symbol-name '#:mop)))))

;;; EOF
