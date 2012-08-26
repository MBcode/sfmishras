;;; -*- Mode: LISP; Syntax: ANSI-COMMON-LISP; Package: CL-USER; Base: 10 -*-

#|
Copyright (c) 2000-2006, Sunil Mishra
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

;; $Id: package.lisp 97 2006-08-31 06:00:10Z smishra $

(in-package :cl-user)

;;; Package definition for the Meta Circular Pattern Matcher.

(defpackage :mcpat
  (:use :common-lisp)
  (:export #:clear-ruleset		; Application functions & macros
	   #:defrule
	   #:in-ruleset
	   #:make-ruleset
	   #:solve
	   #:with-search-rulesets
	   #:encoded-ruleset		; Ruleset classes
	   #:priority-ruleset
	   #:defrule-compiler		; Compiler extension functions & macros
	   #:with-compilation
	   #:mcpat-error		; Conditions
	   #:mcpat-solve-error
	   #:mcpat-solve-failure
	   #:solve-return-default	; Restarts
	   #:solve-return-value
	   ))

;;; EOF
