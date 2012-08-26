;;; -*- Mode: LISP; Syntax: ANSI-COMMON-LISP; Package: PSYS; Base: 10 -*-

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

;;; $Id: system-standalone.lisp 97 2006-08-31 06:00:10Z smishra $

(defvar *source-file-extensions*
  (list "lisp" "cl" "lsp"))
(defvar *compiled-file-extension*
  #.(pathname-type (compile-file-pathname "foo")))

(defun file-newer-p (file1 file2)
  (> (or (file-write-date file1) 0) (file-write-date file2)))

(defun find-file-with-type (pathname types
			    &optional (if-does-not-exist :error))
  (or (cond ((pathname-type pathname)
	     ;; Must be the full file path
	     (when (probe-file pathname)
	       pathname))
	    ((stringp types)
	     (let ((pathname (make-pathname :type types :defaults pathname)))
	       (when (probe-file pathname)
		 pathname)))
	    (t (dolist (source-type types)
		 (let ((pathname
			(make-pathname :type source-type :defaults pathname)))
		   (when (probe-file pathname)
		     (return pathname))))))
      (ecase if-does-not-exist
	((nil :soft) nil)
	(:error (error "Source file corresponding to ~S does not exist."
		       pathname)))))

#-allegro
(defun compile-file-if-needed (source fasl verbose verbose-supplied-p
			       print print-supplied-p external-format force)
  #+clisp (declare (ignore external-format))
  (when (or force
	    (not (probe-file fasl))
	    (and source (file-newer-p source fasl)))
    (loop
     (multiple-value-bind (output-truename warnings-p failure-p)
	 (compile-file source :output-file fasl
		       :verbose (if verbose-supplied-p
				    verbose
				  *compile-verbose*)
		       :print (if print-supplied-p
				  print
				*compile-print*)
		       #-clisp :external-format #-clisp external-format)
       (if (or failure-p (and warnings-p (eql *break-on-signals* t)))
	   ;; Todo: Also need a skip compilation restart.
	   (cerror "Retry compile." "Problems compiling ~S." source)
	 (return output-truename))))))

(defun compile-load (file &key (verbose nil verbose-supplied-p)
		     (print nil print-supplied-p) (if-does-not-exist :error)
		     (external-format :default) output-file force)
  "Compile a file if source newer, and load."
  (let* ((source (find-file-with-type (pathname file) *source-file-extensions*
				      nil))
	 (fasl (apply #'compile-file-pathname (or source file)
		      (when output-file
			`(:output-file ,output-file)))))
    (cond ((or source (probe-file fasl))
	   (let ((compile-result
		  #+allegro
		  (apply #'compile-file-if-needed source
			 `(,@(when verbose-supplied-p `(:verbose ,verbose))
			     ,@(when print-supplied-p `(:print ,print))
			     ,@(when output-file `(:output-file ,output-file))
			     :external-format ,external-format
			     :force-recompile ,force))
		  #-allegro
		  (compile-file-if-needed
		   source fasl verbose verbose-supplied-p print
		   print-supplied-p external-format force)))
	     (values
	      (load fasl :print (if print-supplied-p print *load-print*)
		    :verbose (if verbose-supplied-p verbose *load-verbose*)
		    :if-does-not-exist if-does-not-exist)
	      compile-result)))
	  (t
	   (case if-does-not-exist
	     (:error (error "Could not locate source or fasl for ~S." file))
	     ((nil :soft) nil))))))

(defparameter *MCPAT-FILES*
    '("package"
      "defs"
      "rule-engine"
      "rule-compiler"
      "meta"))

(eval-when (:load-toplevel :execute)
  (defvar *mcpat-root* (make-pathname :name nil :type nil :defaults *load-truename*)))

(defun load-mcpat ()
  (dolist (f *mcpat-files*)
    (let* ((rooted-name (merge-pathnames f *mcpat-root*))
	   (source-file (find-file-with-type rooted-name *source-file-extensions*))
	   (compiled-file (compile-file-pathname rooted-name)))
      (if (file-newer-p compiled-file source-file)
	  (load compiled-file)
	(load source-file)))))

(defun compile-mcpat ()
  (dolist (f *mcpat-files*)
    (compile-load (merge-pathnames f *mcpat-root*))))

;;; EOF
