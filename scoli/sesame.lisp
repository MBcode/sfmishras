;;; -*- Mode: Lisp; Package: SCOLI; Syntax: Ansi-Common-Lisp; Base: 10 -*-

;;; This program is open source.  For license terms, see the LICENSE file.

;;; $Id: sesame.lisp 133 2009-03-01 04:11:11Z smishra $

(in-package "SCOLI")

;;;----------------------------------------
;;; Basic Definitions

(defconstant +sesame-operations+
  '((:protocol . "/protocol")
    (:repositories . "/repositories")
    (:query . "/repositories/~A")
    (:statements . "/repositories/~A/statements")
    )
  "URL patterns for various requests to the sesame REST API.")

(defun default-sesame-query-language ()
  "The default language by which a sesame connection should query the
  repository. For use when initializing a sesame-connection."
  :sparql)

(defun default-sesame-content-language ()
  "The default language in which a sesame connection should communicate
  documents to and from the repository. This language is used for both
  sending and receiving documents containing triples. Used when
  initializing sesame-connection."
  :rdfxml)

(defclass sesame-connection ()
  (;; Logger for future
   ;; (logger)
   (base-url :initarg :base-url :reader base-url
	     :documentation "The root URL for the sesame repository.")
   (repository :initarg :repository :reader sesame-repository
	       :documentation "The repository of interest for this connection.")
   (query-language :initarg :query-language :initform (default-sesame-query-language)
		   :accessor query-language
		   :documentation "The language by which the repository should be queried.")
   (content-language
    :initarg :content-language :initform (default-sesame-content-language)
    :accessor content-language
    :documentation "The default language to communicate documents to the repository."))
  (:documentation "Class that represents a connection to a sesame triple
  store. We manage here many of the defaults desired for calls to sesame."))

(define-condition sesame-error (simple-error)
  ()
  (:documentation "Class for reporting sesame errors."))

;;;----------------------------------------
;;; String Utilities

;;; Whitespace

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +whitespace-chars+
    '#.(remove-duplicates '(#\space #\return #\linefeed #\tab #\newline))
    "Characters defined as whitespace."))

(defun whitespace-char-p (char)
  "Predicate that succeeds if the input object is a whitespace character."
  (case char
    (#.+whitespace-chars+ t)
    (t nil)))

(defun whitespace-string-p (str)
  "Predicate that succeeds if the input object is a string of only
  whitespace characters."
  (and (stringp str)
       (every #'whitespace-char-p str)))

(defun tree-remove-whitespace-strings (tree)
  "Filter out whitespace strings from a cons tree."
  (cond ((whitespace-string-p tree) nil)
	((atom tree) tree)
	(t
	 (let ((tree-car (car tree))
	       (tree-cdr (cdr tree)))
	   (if (whitespace-string-p tree-car)
	       (tree-remove-whitespace-strings tree-cdr)
	       (cons tree-car (tree-remove-whitespace-strings tree-cdr)))))))

;;; ----------------------------------------
;;; XML Utilities

(defun has-attribute-with-value (ex-namespace ex-lname ex-value source)
  "Tests if the source currently points to a set of attribute values that
  includes the given attribute (with namespace) and value. Generally, the
  namespace would be nil. We assume the source is pointing to a start tag
  where attribute values would be present."
  (klacks:map-attributes (lambda (namespace lname qname value specified-p)
			   (declare (ignore qname specified-p))
			   (when (and (eq namespace ex-namespace)
				      (string= lname ex-lname)
				      (string= value ex-value))
			     (return-from has-attribute-with-value t)))
			 source)
  nil)

;;;----------------------------------------
;;; sparql parsing

(defconstant +sparql-namespace+ "http://www.w3.org/2005/sparql-results#"
  "The sparql namespace URL.")
(defconstant +xsd-boolean+ "http://www.w3.org/2001/XMLSchema#boolean"
  "URL for the boolean type declared in XSD.")
(defconstant +xsd-string+ "http://www.w3.org/2001/XMLSchema#string"
  "URL for the string type declared in XSD.")

(defun sparql-parse-variables (source)
  "Parse the variable names declared in the sparql result header."
  (let ((result nil))
    (unless (klacks:find-element source "head" +sparql-namespace+)
      (error 'sesame-error
	     :format-control "Invalid sparql respose: could not find element head."))
    (klacks:consume source)
    (loop
      (multiple-value-bind (ev-type ev-ns ev-lname ev-qname)
	  (klacks:find-event source :start-element)
	(declare (ignore ev-qname))
	(assert (eq ev-type :start-element))
	(unless (and (string= ev-ns +sparql-namespace+) (string= ev-lname "variable"))
	  (return result)))
      (unless (klacks:find-element source "variable" +sparql-namespace+)
	(error 'sesame-error
	       :format-control "Could not find variable element. Something went wrong."))
      (block nil
	(klacks:map-attributes (lambda (namespace lname qname value specified-p)
				 (declare (ignore namespace qname specified-p))
				 (when (string= lname "name")
				   (push value result)
				   (return)))
			       source)
	(error 'sesame-error
	       :format-control "Unable to locate variable in variable declaration block."))
      (klacks:consume source))))

(defun sparql-xmls-for-tag-p (e req-lname)
  "Tests if the xmls encoding presented employs a tag with the given local
  name in the sparql namespace."
  (and (consp e)
       (let ((tag (car e)))
	 (and (consp tag)
	      (ignore-errors
		(destructuring-bind (lname . ns) tag
		  (and (string= lname req-lname)
		       (string= ns +sparql-namespace+))))))))

(defun sparql-parse-literal-xmls (e)
  "Given an XMLS structure representing a sparql literal value, fetch the
  equivalent lisp value from the structure."
  (let ((type-str (when (cadr e)
		    (cadr (assoc "datatype" (cadr e) :test #'string=))))
	(value-list (tree-remove-whitespace-strings (cddr e))))
    ;; XXX I don't know the grammar, so I'll put in a guard
    (assert (null (cdr value-list)) ()
	    "Non-simple value, must extend code.")
    (cond ((or (null type-str) (string= type-str +xsd-string+))
	   (car value-list))
	  ((string= type-str "boolean")
	   (string-equal (car value-list) "true"))
	  (t (error 'sesame-error
		    :format-control "Unrecognized type specifier ~A in ~A."
		    :format-arguments (list type-str e))))))

(defun sparql-gather-values-from-result-xmls (re variables)
  "Gather all the results from the result XML provided in re. If variables
  is nil, then bindings for all variables are returned. Otherwise only
  bindings for the specified variables are returned. The return value is an
  alists keyed on the variable names. The alist thus represents a single
  full result."
  ;; TODO Implement a return pattern.
  (flet ((gather-variable-p (name)
	   (or (null variables)
	       (member name variables :test #'string=))))
    (loop with name
	  for e in (cddr re)
	  when (and (sparql-xmls-for-tag-p e "binding")
		    (gather-variable-p (setq name (cadr (assoc "name" (cadr e) :test #'string=)))))
	    collect (cons name (loop for v in (cddr e)
				     when (sparql-xmls-for-tag-p v "literal")
				       return (sparql-parse-literal-xmls v))))))

;;;----------------------------------------
;;; Request/Response Parameters

(defun url-for-request-type (connection type &rest args)
  "Returns a URL pattern for the given request type for the given
  connection. If supplied, args are applied to the URL pattern. It is up
  to the caller to recognize when and how many arguments are required."
  (let ((url-for-type (cdr (or (assoc type +sesame-operations+)
			       (error 'unknown-request-type :connection connection :type type)))))
    (if args
	(with-standard-io-syntax
	    (format nil "~A~?" (base-url connection) url-for-type args))
	(concatenate 'string (base-url connection) url-for-type))))

(defgeneric language-string (obj format)
  (:documentation "Returns the string representation of the format suitable
  for transmitting in a parameter value to the server. If format is
  :DEFAULT, we query the connection object for the actual format.")
  (:method ((obj sesame-connection) (format (eql :default)))
    (language-string obj (query-language obj)))
  (:method ((obj sesame-connection) format)
    (ecase format
      (:sparql "sparql")
      (:serql "serql"))))

(defgeneric language-mime-type (obj format)
  (:documentation "Returns the mime type string for the format. This mime
  type is suitable to send in the HTTP header, including the Accept and
  Content-Type headers. If format is :DEFAULT, we query the connection
  object for the actual format.")
  (:method ((obj sesame-connection) (format (eql :default)))
    (language-mime-type obj (content-language obj)))
  (:method ((obj sesame-connection) format)
    (ecase format
      (:text "text/plain")
      (:sparql "application/sparql-results+xml")
      (:rdfxml "application/rdf+xml"))))

;;;----------------------------------------
;;; Request/Response

(defgeneric sesame-request (obj url &rest args &key &allow-other-keys)
  (:documentation "Sends a sesame request to the server connection in
  obj. url is the request to be made to the server. Additional arguments,
  such as the format and the parameters, are expected to be provided as
  keyword values as directed by the individual methods.")
  (:method ((obj sesame-connection) url &rest args
	    &key parameters method body request-mime-type response-mime-type
	    &allow-other-keys)
    "Sends a request to the given connection. Does not check if the set of
    keyword arguments provided work together in a sensible manner. That is
    the caller's responsibility."
    (declare (ignore args))
    (apply #'drakma:http-request url :user-agent "SCOLI"
	   (nconc (when request-mime-type
		    (list :content-type request-mime-type))
		  (when response-mime-type
		    (list :accept response-mime-type))
		  (when parameters
		    (list :parameters parameters))
		  (when method
		    (list :method method))
		  (when body
		    (list :content body))))))

(defgeneric parse-response (obj format result &rest args &key &allow-other-keys)
  (:documentation "Parse the response received from the sesame server that
  is connected via obj. The response format and the raw result content is
  provided. Other essential information can be provided through keyword
  arguments.")
  (:method :around ((obj sesame-connection) format (result string) &rest args
		    &key &allow-other-keys)
    (declare (ignore args format))
    ;; TODO Add error checking
    (call-next-method))
  (:method ((obj sesame-connection) (format (eql :null)) result
	    &rest args &key &allow-other-keys)
    (declare (ignore result args))
    nil)
  (:method ((obj sesame-connection) (format (eql :text)) (result string)
	    &rest args &key &allow-other-keys)
    (declare (ignore args))
    result)
  (:method ((obj sesame-connection) (format (eql :sparql)) result &rest args
	    &key variables &allow-other-keys)
    "Extract variable values contained in result. The result is in XML,
    either as a string or some equivalent. From this result, we gather a
    list of alists, each of which holds a value for each variable in the
    result. If variables are specified, only their values collected."
    (declare (ignore args))
    ;; XXX To make this more efficient one could have a response pattern
    (klacks:with-open-source (ksource (cxml:make-source result))
      (let (declared-variables)
	(when (and variables
		   (not (subsetp variables
				 (setq declared-variables (sparql-parse-variables ksource))
				 :test #'string=)))
	  (error 'sesame-error
		 :format-control "~A is not among the variables ~A declared in the result."
		 :format-arguments (list variables declared-variables))))
      (klacks:find-element ksource "results" +sparql-namespace+)
      ;; XXX Quick and dirty. Since we potentially want everything in the results,
      ;; I'll get an xmls representation.
      (let ((results-xmls (klacks:serialize-element ksource (cxml-xmls:make-xmls-builder))))
	(loop for elem in (cddr results-xmls)
	      when (sparql-xmls-for-tag-p elem "result")
		collect (sparql-gather-values-from-result-xmls elem variables)))))
  (:method ((obj sesame-connection) (format (eql :rdfxml)) result &rest args
	    &key &allow-other-keys)
    (declare (ignore args))
    (let ((rdf-result nil))
      (cl-rdfxml:parse-document (lambda (s p o) (push (list s p o) rdf-result)) result)
      rdf-result)))

;;;----------------------------------------
;;; API

(defgeneric get-repositories (obj)
  (:documentation "Get a list of repositories hosted at the server instance.")
  (:method ((obj sesame-connection))
    (let ((result (sesame-request obj (url-for-request-type obj :repositories)
				  :response-mime-type (language-mime-type obj :sparql))))
      ;; XXX To make this more efficient one could have a response pattern
      (loop for var-values in (parse-response obj :sparql result :variables '("id"))
	    for var-value = (car var-values) ; Only one variable
	    collect (cdr var-value)))))

(defun connect-sesame (base-url repository &optional query-language)
  "Create a connection to the sesame repository at base-url. We connect to
  the specified repository. If query-language is specified, all server
  calls are sent in this language by default."
  (let ((c (apply #'make-instance 'sesame-connection :base-url base-url :repository repository
		  (append (when query-language
			    (list :query-language query-language))))))
    (unless (member repository (get-repositories c) :test #'string=)
      ;; Signal an error since we can't create a repository.
      (cerror "Use repository anyway."
	      'sesame-error
	      :format-control "~A is not a valid repository for ~A."
	      :format-arguments (list repository c)))
    c))

(defgeneric get-protocol (obj)
  (:documentation "Retrieve the sesame protocol version.")
  (:method ((obj sesame-connection))
    (let ((result (sesame-request obj (url-for-request-type obj :protocol)
				  :response-mime-type (language-mime-type obj :text))))
      (parse-response obj :text result))))

(defgeneric repository-query (obj query &rest args &key infer-p &allow-other-keys)
  (:documentation "Make a query to the repository. obj is the connection to
  the repository. query is a query object. If not a string, it must be
  appropriately translated. We again allow arbitrary keyword arguments to
  support variations on the basic query method. infer-p indicates that the
  triple store should infer additional results as warranted by the triple
  store.")
  (:method ((obj sesame-connection) (query string) &rest args
	    &key (query-language (query-language obj)) (content-language (content-language obj))
	         infer-p bindings
	    &allow-other-keys)
    (declare (ignore args))
    (let ((req-url-string (url-for-request-type obj :query (sesame-repository obj))))
      (parse-response
       obj
       content-language
       (sesame-request obj req-url-string
		       :response-mime-type (language-mime-type obj content-language)
		       :parameters `(("query" . ,query)
				     ("queryLn" . ,(language-string obj query-language))
				     ,@(when infer-p
					 (list '("infer" . "true")))
				     ,@(when bindings
					 (mapcar (lambda (binding)
						   (destructuring-bind (var val) binding
						     (cons (string var) val)))
						 bindings))))))))

(defgeneric repository-retrieve (obj &rest args &key &allow-other-keys)
  (:documentation "Retrieve content from the given repository, filtering
  based on the supplied keyword arguments.")
  (:method ((obj sesame-connection) &rest args
	    &key subject predicate object context infer-p
	         (content-language (content-language obj))
	    &allow-other-keys)
    (declare (ignore args))
    (flet ((format-context (context)
	     ;; Possible values: bnode, uri, :null
	     ;; TODO How do we recognize a bnode?
	     (cond ((eq context :null) "null")
		   (t (ntriple:stringify context)))))
      (let ((req-url-string (url-for-request-type obj :statements (sesame-repository obj))))
	(parse-response
	 obj
	 content-language
	 (let ((parameters (nconc
			    (when subject (list (cons "subj" (ntriple:stringify subject))))
			    (when predicate (list (cons "pred" (ntriple:stringify predicate))))
			    (when object (list (cons "obj" (ntriple:stringify object))))
			    (when context (list (cons "context" (format-context context))))
			    (when infer-p (list (cons "infer" "true"))))))
	   (sesame-request obj req-url-string
			   :response-mime-type (language-mime-type obj content-language)
			   :parameters parameters)))))))

(defgeneric repository-store (obj content &rest args &key &allow-other-keys)
  (:documentation "Stores content in the sesame repository connected in
  obj. content contains the triples to store. Keyword arguments are used to
  specify other details, such as the specific language of the content.")
  (:method ((obj sesame-connection) (content string) &rest args
	    &key (content-language (content-language obj))
	    &allow-other-keys)
    (declare (ignore args))
    (let ((req-url-string (url-for-request-type obj :statements (sesame-repository obj))))
      (parse-response
       obj
       :null
       (sesame-request obj req-url-string
		       :method :post
		       :request-mime-type (language-mime-type obj content-language)
		       :body content))))
  (:method ((obj sesame-connection) (content pathname) &rest args
	    &key (content-language (content-language obj))
	    &allow-other-keys)
    (declare (ignore args))
    (let ((req-url-string (url-for-request-type obj :statements (sesame-repository obj))))
      (parse-response
       obj
       :null
       (sesame-request obj req-url-string
		       :method :post
		       :request-mime-type (language-mime-type obj content-language)
		       :body content)))))

;;; EOF
