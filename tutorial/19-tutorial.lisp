(defpackage #:clog-tut-19
  (:use #:cl #:clog)
  (:export start-tutorial))

(in-package :clog-tut-19)

;;; In this tutorial we will see how to easily use a JavaScript
;;; component. In the static-files directory there is a simple
;;; JavaScript component (clog/static-files/tutorial/jslists) to create
;;; collapsible trees that we will use for this tutorial.

(defun on-new-window (body)
  ;; First we need to load jslists' JavaScript file and css
  (load-css (html-document body) "/tutorial/jslists/jsLists.css")
  (load-script (html-document body) "/tutorial/jslists/jsLists.js")	    
  ;; Second we need to build an example list. jsLists uses an ordered
  ;; or unordered list for its data.
  (let* ((list-top   (create-unordered-list body))
	 (item       (create-list-item list-top :content "Top of tree"))
	 (list-b     (create-unordered-list item))
	 (item       (create-list-item list-b :content "Item 1"))
	 (item       (create-list-item list-b :content "Item 2"))
	 (item       (create-list-item list-b :content "Item 3"))
	 (item       (create-list-item list-b :content "Item 4")))
    (declare (ignore item))
    (js-execute body (format nil "JSLists.applyToList('~A', 'ALL');"
			     (html-id list-top))))
  (run body))

(defun start-tutorial ()
  "Start tutorial."
  (initialize 'on-new-window)
  (open-browser))
