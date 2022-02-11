;;;; In this tutorial we are going to use clog-web for a website.
;;;; It is possible, for many ideal, to only use CLOG when needed
;;;; for app oriented interactions. I find that I enjoy building
;;;; websites as apps and that is what CLOG-WEB is meant to help with.
;;;; This entire site could have been done in html and just adding
;;;; the boot.js file and then connecting to those form elements or
;;;; other areas desired to bring to life the page. CLOG is
;;;; completely flexible in how you choose to use it.


(defpackage #:clog-tut-26
  (:use #:cl #:clog #:clog-web)
  (:export start-tutorial))

(in-package :clog-tut-26)

(defun on-new-window (body)
  (clog-web-initialize body)
  (setf (title (html-document body)) "Tutorial 26")
  ;; Install a menu
  (let* ((menu  (create-web-menu-bar body))
	 (icon  (create-web-menu-icon menu :on-click (lambda (obj)
						       (declare (ignore obj))
						       (setf (hash (location body)) "rung2"))))
	 (item1 (create-web-menu-item menu :content  "item1"
					   :on-click (lambda (obj)
						       (declare (ignore obj))
						       (setf (hash (location body)) "rung2"))))
	 (item2 (create-web-menu-item menu :content  "item2"
					   :on-click (lambda (obj)
						       (declare (ignore obj))
						       (setf (hash (location body)) "rung2"))))
	 (item3 (create-web-menu-item menu :content  "item3"
					   :on-click (lambda (obj)
						       (declare (ignore obj))
						       (setf (hash (location body)) "rung2"))))
	 (about (create-web-menu-item menu :content  "About"
					   :on-click (lambda (obj)
						       (declare (ignore obj))
						       (setf (hash (location body)) "rung2")))))
    (declare (ignore icon))
    (full-row-on-mobile item1)
    (full-row-on-mobile item2)
    (full-row-on-mobile item3)
    (add-class about "w3-right"))
  ;; rung-1
  (let* ((first-rung (create-web-compositor body :html-id "rung1"))
	 (image      (create-img first-rung :url-src "/img/windmills.jpg"
					    :class "w3-sepia"))
	 (clog-txt   (create-div first-rung :content "CLOG<br><u>The omnificient gui</u><br>
                                                      desktop<br>web<br>mobile"
					    :class   "w3-text-white w3-xlarge")))
    (setf (cursor clog-txt) :pointer)
    (set-on-click clog-txt (lambda (obj)
			     (declare (ignore obj))
			     (setf (display first-rung) :none)
			     (setf (hash (location body)) "rung2")))
    (setf (box-width image) "100%")
    (setf (text-shadow clog-txt) "2px 2px black")
    (composite-top-left clog-txt :padding-class :padding-64))
  ;; rung-2
  (let* ((second-rung (create-web-auto-row body :html-id "rung2"))
	 (image-cell  (create-web-auto-column second-rung))
	 (image       (create-img image-cell :url-src "/img/flower-clogs.jpg"))
	 (text-cell   (create-web-auto-column second-rung :class "w3-cell-top")))
    (hide-on-small-screens image-cell)
    (setf (background-color text-cell) (rgb 199 188 160))
    (setf (box-width image-cell) "40%")
    (setf (box-width image) "100%")
    (clog-web-form text-cell
		   "<H2>Find out more about CLOG:</H2>"
		   '(("CLOG for" :clog-for :select (("Desktop" "desktop" :selected)
						    ("Web"     "web")
						    ("Mobile"  "mobile")
						    ("iot"     "iot")))
		      ("Name"   :name)
		      ("E-mail" :email))
		   (lambda (data)
		     (if (equal (cadr (assoc :email data)) "")		 
			 (clog-web-alert second-rung "Missing E-Mail"
					 "Please fill out E-mail" :time-out 2)
			 (progn
			   (setf (display second-rung) :none)
			   (setf (hash (location body)) "rung3")
			   (setf (inner-html (attach-as-child body "rung3-answer"))
				 (format nil "<br><br>Thank you ~A<br>Your information will
                                                NOT be sent shortly.(DEMO)"
					 (cadr (assoc :name data)))))))))
  ;; rung-3
  (let* ((third-rung (create-web-compositor body :html-id "rung3"))
	 (image      (create-img third-rung :url-src "/img/yellow-clogs.jpg"))
	 (txt        (create-div third-rung :html-id "rung3-answer"
					    :class   "w3-text-white w3-xlarge")))
    (setf (text-shadow txt) "2px 2px black")
    (composite-right txt :padding-class :padding-64)
    (setf (box-width image) "100%"))
  (run body))

(defun start-tutorial ()
  "Start turtorial."
  ;; We would probably set :host to my IP and :port 80 here if running a live site
  (initialize 'on-new-window)
  ;; In real life, if we openning a browser here it would likely be
  ;; to a page with a monitor of system etc. since it is local.
  (open-browser))
