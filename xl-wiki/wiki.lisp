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

(defun current-page ()
  "Extracts the current page from the URL. 
Returns *WIKI-HOME-PAGE* when / is accessed."
  (let ((url (tbnl:script-name)))
    (tbnl::url-decode (if (string= "/" url)
                          *wiki-home-page*
                          (subseq url (+ 1 (position #\/ url :from-end t)))))))

(defun page-version (page)
  "Current version of PAGE in *PAGE-INDEX*"
  (tbnl-mp:with-lock (*page-index-lock*)
    (gethash page *page-index*)))


(defun contents-of-file (pathname)
  "Returns a string with the entire contents of the specified file."
  (with-open-file (in pathname :direction :input)
    (contents-of-stream in)))

;; From lemonodor's LSP
(defun contents-of-stream (stream)
  "Returns a string with the entire contents of the specified stream."
  (with-output-to-string (contents)
    (let* ((buffer-size 4096)
           (buffer (make-string buffer-size)))
      (labels ((read-chunks ()
                 (let ((size (read-sequence buffer stream)))
                   (if (< size buffer-size)
                       (princ (subseq buffer 0 size) contents)
                       (progn
                         (princ buffer contents)
                         (read-chunks))))))
        (read-chunks)))))

(defun meta (pathname)
  "Reads and returns meta data of the page with given PATHNAME."
  (with-open-file (in pathname :direction :input)
    (read in)))

(defun meta-and-content (pathname)
  "Reads and returns meta data and content of the page with given PATHNAME."
  (with-open-file (in pathname :direction :input)
    (values (read in)
            (contents-of-stream in))))

(defun ncomplete-meta (meta)
  "Completes the META data with a few additional values such as the string
at :LAST-MODIFIED, which is the ISO 8601 representation of the universaltime at
:TIME."
  (nconc meta (list :last-modified (iso-date-time (getf meta :time)))))

(defun index-path ()
  "Filesystem path to page index file."
  (merge-pathnames (make-pathname :name "all" :type "index") *wiki-directory*))

(defun set-page-version (page version)
  "Sets the VERSION of given PAGE in the *PAGE-INDEX*. Don't forget to lock!"
  (setf (gethash page *page-index*) version)
  (with-open-file (out (index-path) :direction :output :if-exists :supersede)
    (loop for key being the hash-keys in *page-index* using (hash-value value)
          do (format out "~A~%~A~%" key value))))

(defgeneric save (server page)
  (:documentation "Save the submitted PAGE and display it.")
  (:method ((server wiki) page)
    (tbnl-mp:with-lock (*page-index-lock*)
      (let* ((version (aif (gethash page *page-index*)
			   (1+ it)
			   1))
	     (path (page-version-path page version)))
	(ensure-directories-exist path)
	(with-open-file (out path :direction :output :if-exists :supersede)
	  (prin1 (list :address (if *behind-proxy-p*
				    (tbnl:header-in "X-Forwarded-For")
				    (tbnl:remote-addr))
		       :user nil	; XXX just a test value
		       :time (get-universal-time))
		 out)
	  (write-string (tbnl:parameter "content") out))
	(set-page-version page version)))
    (display server page (page-version page))))

(defun page-version-path (page version)
  "Filesystem path for given PAGE and VERSION number."
  (merge-pathnames (make-pathname :directory (list :relative page)
                                  :name (princ-to-string version)
                                  :type "page")
                   *wiki-directory*))

(defun page-path (page version)
  "Filesystem path for given PAGE. NIL if non existant."
  (let ((path (page-version-path page version)))
    (when (probe-file path)
      path)))
  
(defgeneric execute-main-template (server page version body &key edit meta)
  (:documentation "Execute the main template with all data needed. EDIT is t if you display the
edit form. The plist META contains the meta data like :TIME for last modified time.")
  (:method ((server wiki) page version body &key edit meta)
    (emb:execute-emb (merge-pathnames "main.template" *wiki-template-directory*)
		     :env `(:edit ,edit
			    :page-list ,*wiki-page-list*
			    :edit-url ,(format nil "~A?version=~D&action=edit"
					       page version)
			    :prev-link ,(when (and version (> version 1))
					  (format nil "~A?version=~D"
						  page (1- version)))
			    :version ,(princ-to-string version)
			    :title ,page
			    :page ,page
			    :meta ,meta
			    :body ,body))))

(defgeneric edit (server page version &key preview)
  (:documentation "Edit the PAGE.")
  (:method ((server wiki) page version &key preview)
    (multiple-value-bind (meta content)
	(aif (page-path page version)
	     (if preview
		 (values (meta it) (tbnl:post-parameter "content"))
		 (meta-and-content it))
	     (values nil 
		     (if preview
			 (tbnl:post-parameter "content")
			 (format nil "Describe ~A here." page)))) ; XXX All texts must be configurable. L10N.
      (let ((body (with-html-output-to-string (s)
		    (when preview
		      (htm 
		       (:h2 "Preview")
		       (:hr)
		       (:strong "This is only a preview! Changes aren't saved!")
		       (:hr)
		       (str (translate server content))
		       (:hr)))
		    (:form :action (format nil "~A" page) :method "POST"
			   (:fieldset
			    (:legend "Edit page")
			    (:p
			     (:textarea :name "content"
					:rows (getf *edit-textarea-size* :rows)
					:cols (getf *edit-textarea-size* :cols)
					(esc content)))
			    (:input :type "submit" :name "action-preview" :value "Preview")
			    (str " ")
			    (:input :type "submit" :name "action-save" :value "Save"))))))
	(execute-main-template server page version body :edit t :meta meta)))))

(defgeneric display (server page version)
  (:documentation "Display PAGE.")
  (:method ((server wiki) page version)
    (let ((path (page-path page version)))
      (if path
	  (multiple-value-bind (meta content)
	      (meta-and-content path)
	    (execute-main-template server page version (translate server content)
				   :meta (ncomplete-meta meta)))
	  (edit server page version)))))

(defun debug-header ()
  "Shows the request header for debugging purpose."
  (with-html-output-to-string (s)
    (:html (:head (:title "Request header - DEBUG - XL-WIKI"))
           (:body (:h1 "Request header")
                  (:table :border 1
                   (:tr
                    (:th "Name") (:th "Value"))
                   (loop for (name . value) in (tbnl:headers-in)
                         do (htm (:tr (:td (str name)) (:td (str value))))))))))

(defgeneric wiki (server)
  (:documentation "Main command dispatching function for XL-WIKI.")
  (:method ((server wiki))
    (let* ((action (tbnl:parameter "action"))
	   (action-save (tbnl:parameter "action-save"))
	   (action-preview (tbnl:parameter "action-preview"))
	   (page (current-page))
	   (max-version (page-version page))
	   (param-version (aif (tbnl:parameter "version")
			       (and it (parse-integer it :junk-allowed t))))
	   (version (if (and param-version (< 0 param-version max-version))
			param-version
			max-version)))
      (cond ((string= "debug-header" action) (debug-header))
	    ((or (string= "edit" action) action-preview)
	     (edit server page version :preview action-preview))
	    (action-save (save server page))
	    (t (display server page version))))))

(defun emb-lock-function (func)
  "Lock function for CL-EMB."
  (tbnl-mp:with-lock (*emb-lock*)
    (funcall func)))

;; -------------------------------------------------------------------

(defun config-pathname ()
  "Get the pathname of the wiki configuration file."
  (merge-pathnames 
   (make-pathname :name "wiki" :type "conf")
   *wiki-software-directory*))

(defun read-config (path)
  (with-open-file (plist-stream path)
    (let ((*read-eval* nil)
          (*package* (find-package :keyword)))
      (read plist-stream))))

(defun get-config-value (config section key &optional default)
  (getf (getf config section) key default))

;; Partially taken from PCL 
(defun string-to-directory-pathname (string)
  (when string
    (let ((pathname (pathname string)))
      (merge-pathnames
       (make-pathname 
	:directory (append (or (pathname-directory pathname) (list :relative))
			   (list (file-namestring pathname)))
	:name      nil
	:type      nil
	:defaults pathname)
       *wiki-software-directory*))))

(defun read-page-index ()
  "Filling *PAGE-INDEX*."
  (tbnl-mp:with-lock (*page-index-lock*)
    (clrhash *page-index*)
    (with-open-file (in (index-path) :direction :input :if-does-not-exist nil)
      (when in
        (loop for key = (read-line in nil nil)
              while key
              for value = (parse-integer (read-line in t))
              do (setf (gethash key *page-index*) value))))))

(defun reinit (&optional (wiki *wiki-server*) (class-override-p t))
  "Reinitialize the wiki instance based on the current configuration. By
  default, we reinitialize the current wiki instance. We assume we don't
  generally want to change the class of the wiki. Changing the class is
  only allowed if class-override-p is true, in which case the class in the
  configuration (if present) is used."
  (let ((conf (read-config (config-pathname))))
    (setf *wiki-directory* (string-to-directory-pathname (get-config-value conf :base :directory))
          *wiki-port* (get-config-value conf :base :port *wiki-port*)
          *wiki-template-directory* (get-config-value conf :base :template-directory *wiki-template-directory*)
          *wiki-page-list* (get-config-value conf :base :page-list *wiki-page-list*)
          *wiki-home-page* (get-config-value conf :base :home-page *wiki-home-page*)
          *behind-proxy-p* (get-config-value conf :base :reverse-proxy *behind-proxy-p*)
          *edit-textarea-size* (get-config-value conf :base :edit-textarea-size *edit-textarea-size*))
    (when class-override-p
      (let ((new-class (get-config-value conf :base :class)))
	(when new-class
	  (change-class wiki new-class)))))

  (read-page-index)

  (setf *attribute-quote-char* #\"
        (html-mode) :sgml
        *escape-char-p* #'(lambda (c) (find c "<>&"))
        emb:*locking-function* 'emb-lock-function
        tbnl:*default-content-type* "text/html; charset=utf-8"
        tbnl:*default-handler* (lambda () (wiki wiki))
        tbnl:*dispatch-table* (list
                               (tbnl:create-folder-dispatcher-and-handler
                                "/_static/" (merge-pathnames
					     "_static/"
					     wiki::*wiki-directory*))
                               'tbnl:default-dispatcher))
  wiki)

(defun init (class class-override-p)
  "Initialize the wiki. Create a wiki instance and configure it. If
  class-override-p is set, then we're allowed to change the class of the
  instantiated wiki based on the wiki configuration."
  (let ((wiki (make-instance class)))
    (reinit wiki class-override-p)
    wiki))

;; -------------------------------------------------------------------

(defun start (&optional (class 'wiki class-supplied-p))
  "Start up wiki. Takes an optional class argument to allow specifying a
  different type of wiki."
  (when *wiki-server*
    (error "Cannot run multiple instances of wiki server."))
  (setq *wiki-server* (init class class-supplied-p))
  (setf (http-server *wiki-server*) (tbnl:start-server :port *wiki-port*)))

(defun stop ()
  "Stop wiki."
  (tbnl:stop-server (http-server *wiki-server*))
  (setq *wiki-server* nil))

;;; EOF
