;;; -*- Mode: Lisp -*-

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

;;; $Id: mcpat.asd 97 2006-08-31 06:00:10Z smishra $

(asdf:defsystem mcpat
    :components ((:file "package")
		 (:file "defs" :depends-on ("package"))
		 (:file "rule-engine" :depends-on ("package" "defs"))
		 (:file "simple-ruleset" :depends-on ("package" "defs"))
		 (:file "encoded-ruleset" :depends-on ("package" "defs"))
		 (:file "priority-ruleset" :depends-on ("package" "defs"))
		 (:file "rule-compiler" :depends-on ("package" "defs" "simple-ruleset"))
		 (:file "meta" :depends-on ("package" "defs" "rule-engine" "rule-compiler")
			;; There is a meta-step required here. Unfortunately we cannot
			;; use the simple :in-order-to ((compile-op (load-source-op "meta")))
			;; Just loading meta.lisp on top of rule-compiler produces an
			;; error, because of the interdependencies in the rules of the
			;; meta version. So we use a more complex meta compilation step
			;; that explicitly compiles as a pre-processing step.
			:perform (compile-op :before (op c)
				   (unless (and (boundp '*meta-compile*)
						*meta-compile*)
				     (let ((*meta-compile* t))
				       (declare (special *meta-compile*))
				       (perform (make-instance 'compile-op) c)
				       (perform (make-instance 'load-op) c)))))))

;;; EOF
