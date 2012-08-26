;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: XL-WIKI; Base: 10 -*-

;;; This software is Copyright (c) Stefan Scholl, 2005-2008.
;;; Stefan Scholl grants you the rights to distribute
;;; and use this software as governed by the terms
;;; of the Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html),
;;; known as the LLGPL.

(in-package :xl-wiki)

;; -----------------------------------------------------------------------------
;; Macros
;; -----------------------------------------------------------------------------

(defmacro aif (test then &optional else)
  "Like IF. But the value of the test form gets bound to IT."
  `(let ((it ,test))
    (if it ,then ,else)))


;; -----------------------------------------------------------------------------
;; Time functions
;; -----------------------------------------------------------------------------

(defun iso-date-time (&optional (time (get-universal-time)))
  "Returns string with date + time according to ISO 8601."
  (multiple-value-bind
        (second minute hour date month year)
      (decode-universal-time time 0)
    (format nil "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
            year month date hour minute second)))
