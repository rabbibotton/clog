(defpackage #:clog-user
  (:use #:cl #:clog)
  (:export start-tutorial))

(in-package :clog-user)

;; In this tutorial we will use a CSS only alternative to bootsrap -
;;   https://www.w3schools.com/w3css/default.asp

(defun on-index (body)
  (load-css (html-document body) "https://www.w3schools.com/w3css/4/w3.css")
  (load-css (html-document body) "https://www.w3schools.com/lib/w3-theme-teal.css")
  (setf (title (html-document body)) "Hello W3.CSS")
  
  (let* ((header (create-section body :header :class "w3-container w3-card w3-theme"))
	 (tmp    (create-section header :h1 :content "Explore Forms"))

	 (data-area (create-div body :class "w3-container"))
	 (tmp (create-hr data-area))
	 
	 ;; This is a traditional "post" form that will submit data
	 ;; to a server. Posts are associated with the "page"  and
	 ;; not with a CLOG connection, so are not ideal.
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
	 ;; to a server. While also associated with the page and
	 ;; not with the CLOG connection, you can get the URL from
	 ;; browser and parse put out the results easily.
	 (fcontainer (create-div data-area :class "w3-container"))
	 (tmp        (create-section fcontainer :h2 :content "Get Form"))
	 (tmp        (create-br fcontainer))
	 (form2      (create-form fcontainer :method :get :action "/page3"))
	 (finput     (create-form-element form2 :input :name "yourname" :label
					  (create-label form2 :content "Enter name:")))
	 (fsubmit    (create-form-element form2 :submit))
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

    (set-on-click fsubmit3
		  (lambda (obj)
		    (setf (hiddenp data-area) t)
		    (place-before footer
				  (create-div body
				    :content (format nil "yourname3 = using NAME-VALUE ~A or VALUE ~A"
						     (name-value form3 "yourname3")
						     (value finput3)))))))
  
  (run body))

;; Globals are used as the post data is delivered in parallel to the new
;; page being loaded. If one must use the post method, they will need
;; a more elaborate scheme of hidden fields and synchroniaztion.
(defvar *uri* nil)
(defvar *params* nil)

(defun on-post (uri params)
  (setf *uri* uri)
  (setf *params* params))

(defun on-page2 (body)
  (create-div body :content *uri*)
  (create-div body :content *params*)
  (create-div body :content (format nil "yourname = ~A"
				    (cdr (assoc "yourname" *params* :test #'equalp))))
  (run body))

(defun on-page3 (body)
  (let ((params (quri:uri-query-params (quri:uri (url (location body))))))
    (create-div body :content params)
    (create-div body :content (format nil "yourname = ~A"
				      (cdr (assoc "yourname" params :test #'equalp)))))
  (run body))

(defun start-tutorial ()
  "Start turtorial."

  (initialize #'on-index)
  (clog-connection:set-on-post #'on-post)
  (set-on-new-window #'on-page2 :path "/page2")
  (set-on-new-window #'on-page3 :path "/page3")
  (open-browser))
