;;;; It this tutorial we will create a Common Lisp CLOG version of the
;;;; plugin from the previous two tutorials.

;;; First we will create a package for our component
(defpackage #:clog-drop-list
  (:use #:cl #:clog)
  (:export clog-drop-list
	   create-drop-list
	   drop-root))

(in-package :clog-drop-list)

(defclass clog-drop-list (clog-unordered-list)
  ((drop-root :accessor drop-root))
  (:documentation "CLOG Drop List object - a collapsable list component"))

(defgeneric drop-root (clog-drop-list)
  (:documentation "Accessor for the drop list root, create clog-list-items
on the drop-root."))
  
(defgeneric create-drop-list (clog-obj &key content class html-id auto-place)
  (:documentation "Create a toggler with CONTENT as the top of tree."))

(defmethod create-drop-list ((obj clog-obj) &key (content "")
					      (class nil)
			                      (html-id nil)
					      (auto-place t))
  (let* ((new-obj (create-unordered-list obj :class class
					     :html-id html-id
					     :auto-place auto-place))
	 (header  (create-list-item new-obj :content content)))    
    (change-class new-obj 'clog-drop-list)
    (setf (drop-root new-obj) (create-unordered-list header))
    (set-on-click header
		  (lambda (obj)
		    (if (hiddenp (drop-root new-obj))
			(setf (hiddenp (drop-root new-obj)) nil)
			(setf (hiddenp (drop-root new-obj)) t))))
    new-obj))

(defpackage #:clog-user
  (:use #:cl #:clog)
  (:export start-tutorial))

(in-package :clog-user)

(defun on-new-window (body)
  (let* ((drop-list  (clog-drop-list:create-drop-list body :content "Top of tree"))
	 (item (create-list-item (clog-drop-list:drop-root drop-list) :content "Item 1"))
	 (item (create-list-item (clog-drop-list:drop-root drop-list) :content "Item 2"))
	 (item (create-list-item (clog-drop-list:drop-root drop-list) :content "Item 3"))
	 (item (create-list-item (clog-drop-list:drop-root drop-list) :content "Item 4"))))
  (run body))

(defun start-tutorial ()
  "Start turtorial."
  (initialize #'on-new-window)
  (open-browser))
