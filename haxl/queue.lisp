;;; -*- Mode: LISP; Package: HAXL -*-

#||
Copyright (c) 2007, Sunil Mishra
All rights reserved.
||#

;;; $Id: queue.lisp,v 1.1 2006/01/30 23:24:49 smishra Exp $

(in-package "HAXL")

;;; Enqueue into the tail, dequeue from the head
(defstruct (queue (:constructor %make-queue))
  "The queue data structure, providing convenient access to the head and
the tail of the queue. The content itself is in a list. The head is the
front of the list, such that the car of the head gives the next element
that would be dequeued. The tail is the back of the list, such that the car
of the tail will give the last element in the queue. In an empty queue, the
head and tail are both nil. In a singleton queue, the head and tail point
to the same cons cell."
  head
  tail)

(defun queue-empty-p (q)
  "Check if the queue is empty."
  (null (queue-head q)))

(defun queue-enqueue (q el)
  "Add an element to the queue. The list in the queue is destructively
modified to introduce the element at the tail end."
  (let ((new-tail (list el)))
    (if (queue-tail q)
	(setf (cdr (queue-tail q)) new-tail
	      (queue-tail q) new-tail)
      (setf (queue-head q) new-tail
	    (queue-tail q) new-tail)))
  q)

(defun queue-dequeue (q)
  "Returns two values: the element retrieved, and whether there was an
element to retrieve. Note that the second value is true exactly when
the queue is not empty."
  (unless (queue-empty-p q)
    (let ((el (pop (queue-head q))))
      (unless (queue-head q)
	(setf (queue-tail q) nil))
      (values el t))))

(defun queue-lookahead (q)
  "Returns two values: the element found, and whether there was an element
  found. Note that the second value is true exactly when the queue is not
  empty."
  (unless (queue-empty-p q)
    (values (car (queue-head q)) t)))

(defun queue-enqueue-all (queue elements)
  "Add a set of elements to the queue."
  (dolist (el elements)
    (queue-enqueue queue el))
  queue)

(defun make-queue (&optional initial-elements)
  "Instantiate a new queue. Optionally takes a list of elements that shall
  be initially added to the queue."
  (queue-enqueue-all (%make-queue) initial-elements))

;;; EOF
