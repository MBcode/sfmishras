;;; -*- Mode: Lisp; Package: HAXL -*-

;;; $Id$

(in-package "HAXL")

;;; General purpose XML manipulation tools

(defconstant *xhtml-ns-string* "http://www.w3.org/1999/xhtml")

;;;----------------------------------------
;;; XPath Style Accessors

(defparameter *registered-namespaces* `(("xhtml" . ,*xhtml-ns-string*)))

(defun expand-registered-namespace (ns)
  (cdr (assoc ns *registered-namespaces* :test #'string=)))

(defun expand-qname (qname)
  (let* ((ns-sep-pos (position #\: qname :test #'char=))
	 (ns (when ns-sep-pos
	       (subseq qname 0 ns-sep-pos)))
	 (local-name (if ns-sep-pos
			 (subseq qname (1+ ns-sep-pos))
		       qname)))
    (values (when ns
	      (or (expand-registered-namespace ns) ns))
	    local-name)))

;;;----------------------------------------
;;; XML Subtree Manipulation

(defun elements-in-subtree (context &optional local-name ns)
  (let ((result nil))
    (labels ((collect (element)
	       (when (dom:element-p element)
		 (when (or (null local-name)
			   (and (equal (dom:namespace-uri element) ns)
				(equal (dom:local-name element) local-name)))
		   (push element result))
		 (dom:map-node-list #'collect (dom:child-nodes element)))))
      (when (dom:document-p context)
	(setq context (dom:document-element context)))
      (collect context)
      (nreverse result))))

(mcpat:in-ruleset :xml-path-expand)

#||


(mcpat:defrule path-with-selector
    (((?* ?path) [ (?* ?selector-path) ])
     :constraints ((not (member '[ ?path))))
  "Return a conditional expression, conditioned on the selector(s),
that operates on the given path."
  (let ((selectors nil))
    (loop with one-selector = nil
	for s-item in ?selector-path
	when (eq s-item '])
	do (push (expand-selector (nreverse one-selector)) selectors)
	   (setq one-selector nil)
	else unless (eq s-item '[)
	do (push s-item one-selector)
	finally (push (expand-selector (nreverse one-selector)) selectors))
    `(when (and ,@selectors) ,@(expand-subpath ?path))))

(defun expand-selector (selector)
  "A selector is a general expression. It can include functions, operators,
paths, etc."
  (mcpat:with-search-rulesets (:xml-path-expand)
    (mcpat:solve selector)))

(defun expand-subpath (subpath)
  (mcpat:with-search-rulesets (:xml-path-expand)
    (mcpat:solve subpath)))

(defun expand-path (path)
  (multiple-value-bind (relative-path absolute-path-p)
      (if (eq (car path) '/)
	  (values (cdr path) t)
	(values path nil))
    (let ((relative-result nil))
      (loop with subpath = nil
	  for component in relative-path
	  if (eq component '/)
	  do (push (expand-subpath (nreverse subpath)) relative-result)
	     (setq subpath nil)
	  else do (push component subpath)
	  finally (push (expand-subpath (nreverse subpath)) relative-result))
      (if absolute-path-p
	  (cons '(nconc (list (if (dom:document-p context)
				  context
				(dom:owner-document context))))
		(nreverse relative-result))
	(nreverse relative-result)))))

||#

(declaim (special *context-var*))

(defun update-xpath-context (body)
  `(setq ,*context-var*
     (loop for context in ,*context-var*
	 nconc ,body)))

(mcpat:defrule xpath-absolute-path
    ((path / . ?relative-path))
  (cons `(setq ,*context-var*
	   (list (if (dom:document-p (car ,*context-var*))
		     (car ,*context-var*)
		   (dom:owner-document (car ,*context-var*)))))
	 (mcpat:solve (cons 'path ?relative-path))))

(mcpat:defrule xpath-relative-path
    ((path . ?relative-path) :constraints ((not (eq (car ?relative-path) '/))))
  (mcpat:solve (cons 'relative-path ?relative-path)))

(mcpat:defrule xpath-multi-step-relative-path
    ((relative-path (?* ?step) / . ?relative-path))
  (cons (update-xpath-context (mcpat:solve ?step))
	(mcpat:solve (cons 'relative-path ?relative-path))))

(mcpat:defrule xpath-single-step-relative-path-with-selector
    ((relative-path (?* ?step) [ . ?selectors))
  ;; ?selector is a list of selectors, potentially
  (let* ((path-body (update-xpath-context (mcpat:solve ?step)))
	 (curr-context *context-var*)
	 (*context-var* (gensym "CONTEXTS")))
    `(,path-body
      (setq ,curr-context
	(loop for context in ,curr-context
	    when (let ((,*context-var* (list context)))
		   ,(mcpat:solve ?selectors))
	    collect context)))))

(mcpat:defrule xpath-single-step-relative-path
    ((relative-path . ?step) :constraints ((not (member '/ ?step))))
  (list (update-xpath-context (mcpat:solve ?step))))

(mcpat:defrule xpath-step-selectors
    (((?* ?selector) ] [ . ?selectors))
  (let ((this-body (mcpat:solve ?selector))
	(rest-body (mcpat:solve ?selectors)))
    (if (eq (car rest-body) 'and)
	`(and ,this-body ,@(cdr rest-body))
      `(and ,this-body ,rest-body))))

(mcpat:defrule xpath-step-terminal-selector
    (((?* ?selector) ]) :constraints ((not (member '[ ?selector))))
  (mcpat:solve ?selector))

(mcpat:defrule descendent-element-accessor
    ((descendent ?qname) :constraints ((stringp ?qname)))
  "Return all the element descendents of the context node with the given qname"
  (multiple-value-bind (ns local-name)
      (expand-qname ?qname)
    `(elements-in-subtree context ,local-name ,ns)))

(mcpat:defrule wild-descendent-element-accessor
    ((descendent *))
  `(elements-in-subtree context))

(mcpat:defrule child-text-accessor
    ((child text ()))
  "Return all the text children of the context node"
  `(loop for node across (dom:child-nodes context)
       when (dom:text-node-p node)
       collect node))

(mcpat:defrule child-element-accessor
    ((child ?qname) :constraints ((stringp ?qname)))
  "Return all the element children of the context node with the given qname"
  (multiple-value-bind (ns local-name)
      (expand-qname ?qname)
    `(loop for node across (dom:child-nodes context)
	 when (and (dom:element-p node)
		   (string= (dom:namespace-uri node) ,ns)
		   (string= (dom:local-name node) ,local-name))
	 collect node)))

(mcpat:defrule wild-child-element-accessor
    ((child *))
  "Return all the element children of the context node."
  `(coerce (dom:child-nodes context) 'list))
    
(mcpat:defrule attribute-accessor
    ((attribute ?attr))
  "Return all matching attributes for the context node"
  (multiple-value-bind (ns local-name)
      (expand-qname ?attr)
    `(let ((attr-node (when (dom:element-p context)
			,(if ns
			     `(dom:get-attribute-ns context ,ns ,local-name)
			     `(dom:get-attribute context ,local-name)))))
       (when attr-node
	 (list attr-node)))))

(mcpat:defrule value-expr-expander
    ((value-expr . ?expr))
  ;; FIXME The relative-path below should be expression. When the full
  ;; expression grammar is implemented, this will need fixing.
  (cond ((consp ?expr)
	 (ecase (car ?expr)
	   ((attribute child) (mcpat:solve (cons 'relative-path ?expr)))))
	((or (constantp ?expr) (symbolp ?expr)) (list ?expr))
	(t (error "Could not expand ~S as a value." ?expr))))

(mcpat:defrule negation-operator
    ((not ?expr))
  (let ((g-val (gensym "val")))
    `(let ((,g-val ,(mcpat:solve (cons 'value-expr ?expr))))
       (loop for context in ,g-val
	     always (or (null context) 
			(and (stringp context) (string= context ""))
			(and (integerp context) (zerop context)))))))

(mcpat:defrule equality-operator
    ((?expr = ?val))
  (let ((g-val (gensym "val")))
    `(loop with ,g-val = ,@(mcpat:solve (cons 'value-expr ?val))
	   for context in ,@(mcpat:solve (cons 'value-expr ?expr))
	   when (equal ,g-val context)
	   collect context)))

(defun expand-xpath-expression (path)
  (let ((*context-var* (gensym "CONTEXTS")))
    (values (mcpat:with-search-rulesets (:xml-path-expand)
	      (mcpat:solve (cons 'path path)))
	    *context-var*)))

(defmacro access-path (obj path)
  (multiple-value-bind (expr var)
      (expand-xpath-expression path)
    `(let ((,var (list ,obj)))
       ,@expr
       ,var)))

;;; EOF
