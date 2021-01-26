(defpackage #:clog-user
  (:use #:cl #:clog)
  (:export start-tutorial))

(in-package :clog-user)

;; In previous tutorials we attached to an html file using bootstrap. For this tutorial we
;; are going to create a bootstrap 4.0 page just using CLOG.
;;
;; You should also check out a CSS only alternative to bootsrap -
;;   https://www.w3schools.com/w3css/default.asp

(defun on-index (body)
  (load-css (html-document body) "https://maxcdn.bootstrapcdn.com/bootstrap/4.5.2/css/bootstrap.min.css")
  ;; Bootstrap requires jQuery but there is no need to load it as so does CLOG so already loaded
  ;; the generic boot.html
  (load-script (html-document body) "https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.12.9/umd/popper.min.js")
  (load-script (html-document body) "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0/js/bootstrap.min.js")

  (setf (title (html-document body)) "Hello Boostrap")
  
  (let* ((nav   (create-section body :nav :class "nav"))
	 (l1    (create-a nav :content "link1" :class "nav-link"))
	 (l2    (create-a nav :content "link2" :class "nav-link"))
	 (l3    (create-a nav :content "link3" :class "nav-link"))
	 (l4    (create-a nav :content "link3" :class "nav-link" :link "/page2"))

	 (jumbo (create-div body :class "jumbotron text-center"))
	 (jname (create-section jumbo :h1 :content "My First Bootstrap Page"))
	 (tmp   (create-p jumbo :content "Resize this responsive page to see the effect!"))

	 (container (create-div body :class "container"))
	 (row       (create-div container :class "row"))

	 (col1 (create-div row :class "col-sm-4"))
	 (tmp  (create-section col1 :h3 :content "Column 1"))
	 (tmp  (create-p col1 :content "Lorem ipsum dolor.."))
	 
	 (col2 (create-div row :class "col-sm-4"))
	 (tmp  (create-section col2 :h3 :content "Column 2"))
	 (tmp  (create-p col2 :content "Lorem ipsum dolor.."))

	 (col3 (create-div row :class "col-sm-4"))
	 (tmp  (create-section col3 :h3 :content "Column 3"))
	 (tmp  (create-p col3 :content "Lorem ipsum dolor..")))

    (set-on-click l1 (lambda (obj)(alert (window body) "Clicked link1")))
    
    (set-on-click l2 (lambda (obj)
		       (let* ((alert (create-div body :class "alert alert-warning alert-dismissible fade show"))
			      (tmp   (create-phrase alert :strong :content "Wow! You clicked link 2"))
			      (btn   (create-button alert :class "close" :content "<span>&times;</span>")))
			 (setf (attribute alert "role") "alert")
			 (setf (attribute btn "data-dismiss") "alert")
			 (place-after nav alert))))
    
    (set-on-click l3 (lambda (obj)(setf (color jname) (rgb 128 128 0)))))
    
  (run body))

(defun on-page2 (body)
  ;; Since page2 is a new browser page we need to reload our bootstrap files.
  (load-css (html-document body) "https://maxcdn.bootstrapcdn.com/bootstrap/4.5.2/css/bootstrap.min.css")
  (load-script (html-document body) "https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.12.9/umd/popper.min.js")
  (load-script (html-document body) "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0/js/bootstrap.min.js")

  (setf (title (html-document body)) "Hello Boostrap - page2")

  (let* ((nav   (create-section body :nav :class "nav"))
	 (l1    (create-a nav :content "link1" :class "nav-link"))
	 (l2    (create-a nav :content "link2" :class "nav-link"))
	 (l3    (create-a nav :content "link3" :class "nav-link"))
	 (l4    (create-a nav :content "page1" :class "nav-link" :link "/"))

	 (jumbo (create-div body :class "jumbotron text-center"))
	 (jname (create-section jumbo :h1 :content "You found Page2"))))

  (run body))

(defun start-tutorial ()
  "Start turtorial."

  (initialize #'on-index)
  (set-on-new-window #'on-page2 :path "/page2")
  (open-browser))
