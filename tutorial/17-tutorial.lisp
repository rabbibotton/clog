(defpackage #:clog-tut-17
  (:use #:cl #:clog)
  (:export start-tutorial))

(in-package :clog-tut-17)

;;; In this tutorial we will use a CSS only alternative to bootstrap -
;;;   https://www.w3schools.com/w3css/default.asp
;;;
;;; It is also a demonstration of how various ways to use HTML Forms
(defun on-index (body)
  ;; Load css files
  (load-css (html-document body) "https://www.w3schools.com/w3css/4/w3.css")
  (load-css (html-document body) "https://www.w3schools.com/lib/w3-theme-teal.css")
  ;; Setup page
  (setf (title (html-document body)) "Hello W3.CSS")
  (let* ((header (create-section body :header :class "w3-container w3-card w3-theme"))
	 (tmp    (create-section header :h1 :content "Explore Forms"))
	 ;; Main area of page
	 (data-area (create-div body :class "w3-container"))
	 (tmp (create-hr data-area))
	 ;; This is a traditional "post" form that will submit data
	 ;; to a server.
	 (fcontainer (create-div data-area :class "w3-container"))
	 (tmp        (create-section fcontainer :h2 :content "Post Form"))
	 (tmp        (create-br fcontainer))
	 (form1      (create-form fcontainer :method :post :action "/page2"))
	 (finput     (create-form-element form1 :input :name "yourname" :label
					  (create-label form1 :content "Enter name:")))
	 (fsubmit    (create-form-element form1 :submit))
	 (tmp        (create-br fcontainer))
	 (tmp (create-hr data-area))
	 ;; This is a traditional "get" form that will submit data
	 ;; to a server.
	 (fcontainer (create-div data-area :class "w3-container"))
	 (tmp        (create-section fcontainer :h2 :content "Get Form"))
	 (tmp        (create-br fcontainer))
	 (form2      (create-form fcontainer :method :get :action "/page3"))
	 (finput     (create-form-element form2 :input :name "yourname" :label
					  (create-label form2 :content "Enter name:")))
	 (fsubmit    (create-form-element form2 :submit))
	 (tmp        (create-br fcontainer))
	 (tmp (create-hr data-area))
	 ;; This is a file upload form that will submit data and files
	 ;; to a server.
	 (fcontainer (create-div data-area :class "w3-container"))
	 (tmp        (create-section fcontainer :h2 :content "File Upload Form"))
	 (tmp        (create-br fcontainer))
	 (form4      (create-form fcontainer :method :post
					     :encoding "multipart/form-data"
					     :action "/page4"))
	 (finput     (create-form-element form4 :file :name "filename"))
	 (fsubmit    (create-form-element form4 :submit))
	 (tmp        (create-br fcontainer))
	 (tmp (create-hr data-area))
	 ;; This is a CLOG style form, instead of submitting data
	 ;; to another page it is dealt with in place.
	 (fcontainer (create-div data-area :class "w3-container"))
	 (tmp        (create-section fcontainer :h2 :content "CLOG Style Form"))
	 (tmp        (create-br fcontainer))
	 (form3      (create-form fcontainer))
	 (finput3    (create-form-element form3 :input :name "yourname3" :label
					  (create-label form3 :content "Enter name:")))
	 (fsubmit3   (create-form-element form3 :submit))
	 (tmp        (create-br fcontainer))
	 (tmp (create-hr data-area))
	 (footer (create-section body :footer :class "w3-container w3-theme"))
	 (tmp    (create-section footer :p :content "(c) All's well that ends well")))
    (declare (ignore tmp) (ignore finput) (ignore fsubmit))

    (set-on-click fsubmit3
      (lambda (obj)
	(declare (ignore obj))
	(setf (hiddenp data-area) t)
	(place-before footer
		      (create-div body
		         :content (format nil "yourname3 = using NAME-VALUE ~A or VALUE ~A"
					  (name-value form3 "yourname3")
					  (value finput3)))))))
  (run body))

(defun on-page2 (body)
  (let ((params (form-post-data body)))
    (create-div body :content params)
    (create-div body :content (format nil "yourname = ~A"
				      (form-data-item params "yourname"))))
  (run body))

(defun on-page3 (body)
  (let ((params (form-get-data body)))
    (create-div body :content params)
    (create-div body :content (format nil "yourname = ~A"
				      (form-data-item params "yourname"))))
  (run body))

(defun on-page4 (body)
  (let ((params (form-multipart-data body)))
    (create-div body :content params)
    (destructuring-bind (stream fname content-type)
	(form-data-item params "filename")
      (create-div body :content (format nil "filename = ~A - (contents printed in REPL)" fname))
      (let ((s (flexi-streams:make-flexi-stream stream :external-format :utf-8))
	    (b (make-string 1000)))
	(loop
	  (let ((c (read-sequence b s)))
	    (unless (plusp c) (return))
	    (princ (subseq b 1 c))))))
    (delete-multipart-data body))
  (run body))

(defun start-tutorial ()
  "Start tutorial."
  (initialize 'on-index)
  (set-on-new-window 'on-page2 :path "/page2")
  (set-on-new-window 'on-page3 :path "/page3")
  (set-on-new-window 'on-page4 :path "/page4")
  (open-browser))
