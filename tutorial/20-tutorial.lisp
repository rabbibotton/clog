;;;; In this tutorial we will see how to wrap last tutorial's JavaScript
;;;; component in to something that feels more like a plugin to CLOG.

;;; First we will create a package for our component
(defpackage #:clog-toggler
  (:use #:cl #:clog)
  (:export clog-toggler
	   init-toggler
	   create-toggler
	   activate))

(in-package :clog-toggler)

;;; Next we will create a function to initialize the environment
;;; for the component.
(defun init-toggler (body &key (path-to-js "/tutorial/jslists/"))
  "Initialize BODY to use clog-toggler components"  
  (load-css (html-document body)
	    (concatenate 'string path-to-js "jsLists.css"))
  (load-script (html-document body)
	       (concatenate 'string path-to-js "jsLists.js")))

;;; Next we will use the clog-unordered-list as the base for our new
;;; class clog-toggler
(defclass clog-toggler (clog-unordered-list) ()
  (:documentation "Toggler object - a collapsible UI component"))

(defgeneric create-toggler (clog-obj &key class html-id auto-place)
  (:documentation "Create a toggler."))

(defmethod create-toggler ((obj clog-obj) &key (class nil)
			                    (html-id nil)
					    (auto-place t))
  (let ((new-obj (create-unordered-list obj :class class
					    :html-id html-id
					    :auto-place auto-place)))
    ;; Using change-class we can re-use the parent clog-unordered-lists's
    ;; create method and its initialization. Otherwise we can use
    ;; create-child and the needed html.
    (change-class new-obj 'clog-toggler)
    new-obj))

(defgeneric activate (clog-toggler)
  (:documentation "Activate the clog-toggler."))

(defmethod activate ((obj clog-toggler))
  (js-execute obj (format nil "JSLists.applyToList('~A', 'ALL');"
			  (html-id obj))))
  
(defpackage #:clog-tut-20
  (:use #:cl #:clog)
  (:export start-tutorial))

(in-package :clog-tut-20)

(defun on-new-window (body)
  (clog-toggler:init-toggler body)
  ;; Now we build our CLOG-Toggler
  ;; All create-functions also allow setting the :html-id instead of
  ;; using a generated id.
  (let* ((toggler    (clog-toggler:create-toggler body :html-id "myid"))
	 (item       (create-list-item toggler :content "Top of tree"))
	 (list-b     (create-unordered-list item))
	 (item       (create-list-item list-b :content "Item 1"))
	 (item       (create-list-item list-b :content "Item 2"))
	 (item       (create-list-item list-b :content "Item 3"))
	 (item       (create-list-item list-b :content "Item 4")))
    (declare (ignore item))
    (clog-toggler:activate toggler))
  (run body))

(defun start-tutorial ()
  "Start tutorial."
  (initialize 'on-new-window)
  (open-browser))
