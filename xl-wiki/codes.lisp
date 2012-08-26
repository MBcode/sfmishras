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

;;; Handling wiki codes

;;; Translation proceeds in three stages:
;;; * Segmentation
;;; * Substitution
;;; * Reconstitution
;;; Each step is mediated by the wiki instance.
;;;
;;; During segmentation, the wiki document is decomposed into a document
;;; tree based on the segments detected.
;;;
;;; During substitution, the wiki codes in each segment are substituted
;;; with HTML equivalents. The substitution also takes into account the
;;; position of the substitution in the document tree.
;;;
;;; During reconstitution, the document tree is turned back into a flat
;;; HTML document.

(defparameter *marker* (map 'string #'code-char #(#x1e #xff #xfe #x1e))
  "To mark the place in the string where preserved HTML should be restored.")

(defun make-rep-url-encoder (reg-index)
  "Generates a replacement function for CL-PPCRE:REGEX-REPLACE-ALL. The
  function url-encodes the match in the specified register 0 based index."
  (lambda (target-string start end match-start match-end reg-starts reg-ends)
    (declare (ignore start end match-start match-end))
    (tbnl:url-encode
     (subseq target-string (aref reg-starts reg-index) (aref reg-ends reg-index)))))

(defun rep-nowiki (target-string start end match-start match-end
                   reg-starts reg-ends)
  "Replacement function for CL-PPCRE:REGEX-REPLACE-ALL. Match in register 1 (0 in array)
will be escaped and stored in *preserved-html*."
  (declare (special *preserved-html*) (ignore start end match-start match-end))
  (prog1
      (format nil "~A~D~A" *marker* (fill-pointer *preserved-html*) *marker*)
    (vector-push-extend
     (escape-string (subseq target-string (or (aref reg-starts 0) 0) (or (aref reg-ends 0) 0)))
     *preserved-html*)))

(defun rep-list (target-string start end match-start match-end reg-starts reg-ends)
  "Replacement function for CL-PPCRE:REGEX-REPLACE-ALL. Handles lists."
  (declare (ignore start end reg-starts reg-ends))
  (with-output-to-string (s)
    (write-string "<ul>" s)
    (cl-ppcre:do-matches-as-strings (line "(?ims)(?:<br>)?.*?(?=(?:<br>|\\z))"
                                          (subseq target-string match-start match-end)
                                          nil :sharedp t)
      (write-string (ppcre:regex-replace "(?:<br>|^)\\*\\s*(.*)" line "<li>\\1</li>") s))
    (write-string "</ul>" s)))

(defun make-source-snippet (lang string)
  "Make a source snippet (colorized or just plain PRE) out of string."
  (if lang
      (concatenate 'string "<div class=\"code\">"
		   (colorize::html-colorization lang string) "</div>")
      (concatenate 'string "<pre class=\"code\">" string "</pre>")))

(defun rep-source (target-string start end match-start match-end
                   reg-starts reg-ends)
  "Replacement function for CL-PPCRE:REGEX-REPLACE-ALL. Match in register 1 (0 in array)
is the language, match in register 2 becomes a source snippet. Stored in *preserved-html*."
  (declare (special *preserved-html*) (ignore start end match-start match-end))
  (prog1
      (format nil "~A~D~A" *marker* (fill-pointer *preserved-html*) *marker*)
    (vector-push-extend
     (let* ((lang-string
	     (subseq target-string (or (aref reg-starts 0) 0) (or (aref reg-ends 0) 0)))
            (lang (first (rassoc lang-string (colorize:coloring-types) :test #'string-equal))))
       (make-source-snippet lang (subseq target-string (aref reg-starts 1) (aref reg-ends 1))))
     *preserved-html*)))

(defun rep-restore-preserved (target-string start end match-start match-end
                              reg-starts reg-ends)
  "Replacement function for CL-PPCRE:REGEX-REPLACE-ALL. Match in register 1
  (0 in array) is an index for the array *preserved-html*. Restores
  preserved HTML."
  (declare (special *preserved-html*) (ignore start end match-start match-end))
  (aref *preserved-html*
	(parse-integer (subseq target-string (aref reg-starts 0) (aref reg-ends 0)))))

(defgeneric pre-escape-rewrites (server)
  (:documentation "List of lists that store named rewrite rules. Each list
  is of the form (<name> <pattern> <replacement>). Pattern is a regexp
  string and replacement encodes a result form suitable for use with
  CL-PPCRE:REGEX-REPLACE-ALL. These rewrites are applied before HTML
  escaping occurs.")
  (:method ((server wiki))
    (load-time-value
     `(
       ;; Remove the marker first, so there will be no trouble at the end.
       (:remove-marker ,*marker* "")
       ;; <nowiki> .. </nowiki> ==> Exclude portions of text from wiki converting
       (:nowiki "(?ims)<nowiki>(.*?)</nowiki>" rep-nowiki)
       ;; <!-- comment --> ==> Removes everything between <!-- and -->. Allows commenting.
       (:comment "(?ims)<!--(.*?)-->" "")
       ;; <source> .. </source> ==> Sourcecode
       (:source "(?ims)<source(?:\\s*lang=\"(.*?)\")?>(?:\\x0d?\\x0a)*(.*?)</source>" rep-source))
     )))

(defgeneric post-escape-rewrites (server)
  (:documentation "List of lists that store named rewrite rules. Each list
  is of the form (<name> <pattern> <replacement>). Pattern is a regexp
  string and replacement encodes a result form suitable for use with
  CL-PPCRE:REGEX-REPLACE-ALL. These rewrites are applied after HTML
  escaping occurs.")
  (:method ((server wiki))
    (load-time-value
     `(
       ;; CR or LF ==> <br>
       (:eol "\\x0d?\\x0a" "<br>")
       ;; <br> or <br/> ==> Start a new line
       (:br "&lt;br\s*/?&gt;(<br>)?" "<br/>")
       ;; '''text''' ==> stronger emphasize text (<strong>text</strong>),
       ;;                on most browsers bold.
       (:strong "'''(.*?)'''" "<strong>\\1</strong>")
       ;; ''text'' ==> emphasize text (<em>text</em>), on most browsers italic.
       (:emphasis "''(.*?)''" "<em>\\1</em>")
       ;; <small>text</small> ==> text in small font
       (:small "&lt;small&gt;(.*?)&lt;/small&gt;" "<small>\\1</small>")
       ;; <big>text</big> ==> text in big font
       (:big "&lt;big&gt;(.*?)&lt;/big&gt;" "<big>\\1</big>")
       ;; <sub>x</sub> ==> subscripting x
       (:sub "&lt;sub&gt;(.*?)&lt;/sub&gt;" "<sub>\\1</sub>")
       ;; <sup>x</sup> ==> superscripting x
       (:sup "&lt;sup&gt;(.*?)&lt;/sup&gt;" "<sup>\\1</sup>")
       ;; <s>text</s> ==> strike out text
       (:strikeout "&lt;s&gt;(.*?)&lt;/s&gt;" "<s>\\1</s>")
       ;; <u>text</u> ==> underline text
       (:underline "&lt;u&gt;(.*?)&lt;/u&gt;" "<u>\\1</u>")
       ;; <del>text</del> ==> Mark text as deleted
       (:deleted "&lt;del&gt;(.*?)&lt;/del&gt;" "<del>\\1</del>")
       ;; <ins>text</ins> ==> Mark text as inserted
       (:inserted "&lt;ins&gt;(.*?)&lt;/ins&gt;" "<ins>\\1</ins>")
       ;; [[Page|Text]] ==> Generates a link to named page and links Text.
       (:link-with-alternate-text "\\[\\[([^]\">|]*?)\\|([^]\">]*?)\\]\\]"
	("<a href=\"" ,(make-rep-url-encoder 0) "\" title=\"" 0 "\">" 1 "</a>"))
       ;; [[Page]] ==> Generates a link to named page.
       (:link "\\[\\[(.*?)\\]\\]"
	("<a href=\"" ,(make-rep-url-encoder 0) "\" title=\"" 0 "\">" 0 "</a>"))
       ;; [http://www.somepage.example/] ==> Inserts external link.
       (:external-link "\\[(http|https|ftp|mailto|gopher):([^\">].*?)\\]"
	"<a rel=\"nofollow\" class=\"external \\1\" href=\"\\1:\\2\" title=\"\\1:\\2\">\\1:\\2</a>")
       ;; ====== Foo ======  ==> Level 6 header (etc.)
       (:header6 "(?ims)(<br>|^)======\\s*(.*?)\\s*=*(?=(?:<br>|\\z))" "\\1<h6>\\2</h6>")
       (:header5 "(?ims)(<br>|^)=====\\s*(.*?)\\s*=*(?=(?:<br>|\\z))" "\\1<h5>\\2</h5>")
       (:header4 "(?ims)(<br>|^)====\\s*(.*?)\\s*=*(?=(?:<br>|\\z))" "\\1<h4>\\2</h4>")
       (:header3 "(?ims)(<br>|^)===\\s*(.*?)\\s*=*(?=(?:<br>|\\z))" "\\1<h3>\\2</h3>")
       (:header2 "(?ims)(<br>|^)==\\s*(.*?)\\s*=*(?=(?:<br>|\\z))" "\\1<h2>\\2</h2>")
       (:header1 "(?ims)(<br>|^)=\\s*(.*?)\\s*=*(?=(?:<br>|\\z))" "\\1<h1>\\2</h1>")
       ;; ---- ==> Horizontal divider
       (:divider "(?ims)(<br>|^)----.*?(?=(?:<br>|\\z))" "\\1<hr>")
       ;; * Item ==> List item
       (:list-item "(?ims)(?:<br>|^)\\*.*?(?=(?:<br>|^)[^\\*]|\\z)" ,'rep-list)
       ;; <br><br><br>* ==> <p>
       (:paragraph "(?ims)(<br>){2,}" "<p>")
       ;; <br/> => "<br>" - Cleanup for <br> wiki tag
       (:br-xml "(?ims)<br/>" "<br>")
       ;; Restore preserved HTML
       (:restore-html ,(concatenate 'string *marker* "(\\d+)" *marker*) ,'rep-restore-preserved)
       ))))

(defvar *translate-context*)

(defgeneric reconstitute (server node-parents doc-tree)
  (:documentation "Take a document tree, substituted, and produce a final
  stitched together HTML document.")
  (:method ((server wiki) node-parents (doc-tree string))
    (declare (ignore node-parents))
    doc-tree)
  (:method ((server wiki) node-parents (doc-tree list))
    (let ((node (car doc-tree)))
      (apply #'concatenate 'string
	     (loop with new-parents = (cons node node-parents)
		for subtree in (cdr doc-tree)
		collect (reconstitute server new-parents subtree))))))

(defgeneric rewrite (server node-parents doc-tree)
  (:documentation "Take a document tree, as produced by SEGMENT, and
  rewrite each of the nodes of the tree.")
  (:method ((server wiki) node-parents (doc-tree string))
    "Translate wiki codes."
    (declare (ignore node-parents))
    (let ((*preserved-html* (make-array 3 :element-type 'string 
					  :adjustable t :fill-pointer 0)))
      (declare (special *preserved-html*)) ; To temporary store HTML code
      ;; First with the unescaped string
      (loop for (nil regexp replace) in (pre-escape-rewrites server)
	 do (setf doc-tree (ppcre:regex-replace-all regexp doc-tree replace)))
      ;; Replacement in the escaped string
      (setf doc-tree (escape-string doc-tree))
      (loop for (nil regexp replace) in (post-escape-rewrites server)
	 do (setf doc-tree (ppcre:regex-replace-all regexp doc-tree replace)))
      doc-tree))
  (:method ((server wiki) node-parents (doc-tree cons))
    (let ((node (car doc-tree)))
      (cons node 
	    (loop with new-parents = (cons node node-parents)
	       for subtree in (cdr doc-tree)
	       collect (rewrite server new-parents subtree))))))

(defgeneric segment (server string)
  (:documentation "Segment a document into a tree based on the markup in
  the document. Each node in the tree is either a string, or a list. The
  head of the list is a node that indicates the context for the rest of the
  document tree, and each child is a subtree.")
  (:method ((server wiki) string)
    string))

(defgeneric make-translate-context (server)
  (:documentation "A translation context to hold temporary state during
  wiki to HTML translation.")
  (:method ((server wiki))
    "The default translation context is empty."
    nil))

(defgeneric translate (server string)
  (:documentation "Translates the wiki codes inside STRING into HTML.")
  (:method ((server wiki) string)
    (let ((*translate-context* (make-translate-context server)))
      (reconstitute server nil (rewrite server nil (segment server string))))))

;;; EOF
