;;; Creating code GUIs from scratch is great, but in the real world often design
;;; is left to artists and code is left to engineers. CLOG is an amazing solution
;;; for that model of app development. Any existing web page can become a CLOG
;;; app by simply adding the following two lines to an existing HTML file:
;;;
;;;      <script src="/js/jquery.min.js" type="text/javascript"></script>
;;;      <script src="/js/boot.js" type="text/javascript"></script>
;;;
;;; The first line adds jquery which CLOG uses to enhance browser compatability.
;;; The second line adds the CLOG "boot" file.
;;;
;;; For this tutorial we generated the clog/static-files/tutorial/tut-11.html
;;; by using the form generator at https://bootsnipp.com/forms?version=3
;;; then used the the template from https://getbootstrap.com/docs/3.3/
;;; and added CLOG's boot.js script line. It was neccesary to add an id to the
;;; form (id='form1') as the generator did not add one.
;;;
;;; In the code below:
;;;
;;;  - We set a blank on-submit to overide the behavior in the bootstrap
;;;          buttons to submit the form HTML style.
;;;  - We are going to attach to the "Good Button" an on-click handler
;;;          to handle getting the values
;;;  - We attach to each control in the on-click handler that we want
;;;          a value for and query them
;;;  - We attach an on-click handler that resets the form to the
;;;          "Scary Button"
;;;
;;;  We go in the browser to the file
;;;          "http://127.0.0.1:8080/tutorial/tut-11.html"

(defpackage #:clog-tut-11
  (:use #:cl #:clog)
  (:export start-tutorial))

(in-package :clog-tut-11)

(defun on-new-window (body)
  ;; This will turn on debug output in the browser console.
  (debug-mode body)
  ;; Setup form
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
	       ;; We collect the data from the hidden form elements
	       ;; using radio-value and name-value (for other types if
	       ;; input other than radio buttons) or we could bind each
	       ;; control (using ATTACH-AS-CHILD)and seek their value
	       ;; directly. See tutorial 17 and to deal with forms in
	       ;; the old html page model of "put" and "get"
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
  "Start tutorial."
  (initialize 'on-new-window)
  (open-browser :url "http://127.0.0.1:8080/tutorial/tut-11.html"))
