;;; CLOG is an excellent choice for websites as well as GUI's for applications.
;;; The first 10 tutorials focused on a single "page" application. For GUIs
;;; that works well, and combining your CLOG app embedded in an native app that
;;; provides a web control on desktop or mobile works well. CLOG apps of course
;;; are web apps right out of the box. However CLOG is also more than capable
;;; of handling things in a more traditional website manner.
;;;
;;; In the last tutorial it was demonstrated that one can take any HTML file
;;; add the boot.js file to it and then it becomes a dynamic interactive
;;; clog app. An entire site could be laid out using .html files and where
;;; desired a fully dynamic page can be created by copying the boot.html file
;;; or some styled html template etc. (Look in the next few demos for
;;; examples using templates like bootstrap with CLOG, etc).
;;;
;;; Here we demonstrate how CLOG routs to dynamic pages. Static pages are
;;; placed in the directory set on initialization.
;;;
;;; See START-TUTORIAL below.

(defpackage #:clog-tut-12
  (:use #:cl #:clog)
  (:export start-tutorial))

(in-package :clog-tut-12)

(defun on-main (body)
  (let ((sb (create-style-block body)))
    (add-style sb :element "a"       '(("color"           :orange)
				       ("text-decoration" :none)))
    (add-style sb :element "a:hover" '(("background-color" :gray))))
  (create-div body :content
     "We are in on-main<br><br>
      <h1>Pick a link</h1>
      <ul>
        <li><a href='/page1'>/page1</a> - a CLOG app
        <li><a href='/page1.html'>/page1.html</a> - a CLOG app mascarading as a .html
        <li><a href='/somepath/hi/'>/somepath/hi/</a> - a CLOG app mascarading as a .html
        <li><a href='/page2'>/page2</a> - a CLOG app using an alternative boot file
        <li><a href='/page3'>/page3</a> - tutorial 11 as part of this tutorial
        <li><a href='/tutorial/tut-11.html'>/tutorial/tut-11.html</a> - an html file using boot.js
        <li><a href='/tutorial/regular-file.html'>'/tutorial/regular-file.html</a> - a regular html file
     </ul>")
  (run body))

(defun on-page1 (body)
  (create-div body :content "You are in on-page1")
  (run body))

(defun on-page2 (body)
  (create-div body :content "You are in on-page2")
  (log-console (window body) "A message in the browser's log")
  (run body))

(defun on-default (body)
  (cond ((equalp (path-name (location body))
		 "/tutorial/tut-11.html")
	 (on-tutorial11 body))
	(t
	 (create-div body :content "No dice!")
	 (run body))))

(defun on-tutorial11 (body)
  (let* ((form         (attach-as-child body "form1" :clog-type 'clog-form))
	 (good-button  (attach-as-child body "button1id"))
	 (scary-button (attach-as-child body "button2id")))
    (flet ((on-click-good (obj)
	     (declare (ignore obj))
	     (let ((alert-div (create-div body)))
	       (place-before form alert-div)
	       (setf (hiddenp form) t)
	       ;; Bootstrap specific markup
	       (setf (css-class-name alert-div) "alert alert-success")
	       (setf (attribute alert-div "role") "alert")
	       (setf (inner-html alert-div)
		     (format nil "<pre>radios value : ~A</pre><br>
                           <pre>textinput value : ~A</pre><br>"
			     (radio-value form "radios")
			     (name-value form "textinput")))))
	   (on-click-scary (obj)
	     (declare (ignore obj))
	     (reset form)))
    ;; We need to override the boostrap default to submit the form html style
    (set-on-submit form (lambda (obj)(declare (ignore obj))()))
    (set-on-click good-button 'on-click-good)
    (set-on-click scary-button 'on-click-scary))
    (run body)))

(defun start-tutorial ()
  "Start turtorial."
  (initialize 'on-main)
  ;; Navigating to http://127.0.0.1:8080/page1 executes on-page1
  (set-on-new-window 'on-page1 :path "/page1")
  ;; Navigating to http://127.0.0.1:8080/page1.html executes on-page1
  ;; There is no .html file - it is just a route to CLOG handler
  ;; but the user thinks it is like any other html file.
  (set-on-new-window 'on-page1 :path "/page1.html")
  ;; Navigating to http://127.0.0.1:8080/somepath/page1/ executes on-page1
  ;; the path set can be any valid html path and has no meaning.
  (set-on-new-window 'on-page1 :path "/somepath/hi/")
  ;; Here we add another page, page2. It uses a boot file that turns
  ;; on debugging to the browser console of communications with the
  ;; server.
  (set-on-new-window 'on-page2 :path "/page2" :boot-file "/debug.html")
  ;; Here we add another page, page3. But this time we use the html file
  ;; from tutorial 11 and make it the boot-file and execute the same code
  ;; in (on-tutorial11) as in tutorial 11.
  (set-on-new-window 'on-tutorial11 :path "/page3"
				     :boot-file "/tutorial/tut-11.html")
  ;; Setting a "default" path says that any use of an included boot.js
  ;; file will route to this function, in this case #'on-default
  ;; which will determine if this is coming from the path used in tutorial
  ;; 11 - "http://127.0.0.1:8080/tutorial/tut-11.html" and if it does
  ;; use on-tutorial11, and if not say "No Dice!"
  (set-on-new-window 'on-default :path "default")
  (open-browser))
