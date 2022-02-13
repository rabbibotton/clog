;;;; Demo 4 - CMS Website

(defpackage #:clog-demo-4
  (:use #:cl #:clog #:clog-web)
  (:export start-demo))

(in-package :clog-demo-4)

;; Site Configuration
(defparameter side-panel-size 200 "Size of menu")
(defparameter sysop-password "admin")

(defvar *sql-connection*)
(defvar *site-config*)

(defclass app-data ()
  ((head
    :accessor head)
   (side
    :accessor side)
   (main
    :accessor main)
   (sysop
    :initform nil
    :accessor sysop)))

(defun create-web-frame (body app)
  (setf (title (html-document body)) "Demo 4")
  ;; +----------------------------------------------+
  ;; |             header area (head)               |
  ;; +----------------------------------------------+
  ;; |          |                                   |
  ;; |   menu   |          content area             |
  ;; |  (side)  |             (main)                |
  ;; |          |                                   |
  ;; +----------+-----------------------------------+
  ;; create 3 pain site
  ;;
  ;; Header
  (setf (head app) (create-web-panel body :content "<h3>Demo 4:</h3><p>A simple Lisp CMS</p>"
					  :class   "w3-yellow"))
  ;; Sidebar
  (setf (side app) (create-web-sidebar body))
  (setf (box-width (side app)) (unit :px side-panel-size))
  (add-card-look (side app))
  ;; Main
  (setf (main app) (create-web-content body))
  (set-margin-side (main app) :left (unit :px (+ side-panel-size 10)))
  (create-web-container (main app))) 

(defun insert-content (app new-page text-area)
  (sqlite:execute-non-query
   *sql-connection*
   (format nil "insert into config (menu, main) values ('~A', '~A')"
	   (escape-string (value new-page))
	   (escape-string (value text-area))))
  (reset-menu app)
  (route-content app (escape-string (value new-page))))
  
(defun new-content (app)
  (setf (inner-html (main app)) "")
  (let ((new-page  (create-form-element (main app) :text :value "New Title"))
	(tmp       (create-br (main app)))
	(text-area (create-text-area (main app) :rows 10 :columns 40)))
    (declare (ignore tmp))
    (create-br (main app))
    (set-on-click (create-button (main app) :content "Insert")
		  (lambda (obj)
		    (declare (ignore obj))
		    (insert-content app new-page text-area)))))  

(defun update-content (app page text-area)
  (sqlite:execute-non-query
   *sql-connection*
   (format nil "update config set main='~A' where menu='~A'"
	   (escape-string (value text-area))
	   page))
  (route-content app page))

(defun delete-content (app page)
  (sqlite:execute-non-query
   *sql-connection*
   (format nil "delete from config where menu='~A'" page))
  (reset-menu app)
  (route-content app "Home"))

(defun edit-content (app page)
  (setf (inner-html (main app)) "")
  (let ((contents (sqlite:execute-to-list
		   *sql-connection*
		   (format nil "select main from config where menu='~A'" page))))
    (dolist (content contents)
      (let ((text-area (create-text-area (main app) :rows 10 :columns 40
						    :value (car content))))
	(create-br (main app))
	(set-on-click (create-button (main app) :content "Update")
		      (lambda (obj)
			(declare (ignore obj))
			(update-content app page text-area)))
	(unless (equal page "Home")
	  (set-on-click (create-button (main app) :content "Delete")
			(lambda (obj)
			  (declare (ignore obj))
			  (delete-content app page))))))))

(defun route-content (app page)
  (setf (inner-html (main app)) "")
  (let ((contents (sqlite:execute-to-list
		   *sql-connection*
		   (format nil "select main from config where menu='~A'" page))))
    (dolist (content contents)
      (setf (inner-html (main app)) (car content))
      (create-br (main app))
      (create-br (main app))
      (when (sysop app)
	(set-on-click (create-a (main app) :content "edit")
		      (lambda (obj)
			(declare (ignore obj))
			(edit-content app page)))))))
(defun id-me (app)
  (setf (inner-html (main app)) "")
  (clog-web-form (main app) "Validate:"
		 '(("Password" "pass" :password))
		 (lambda (res)
		   (if (equal (second (first res)) sysop-password)
		       (progn
			 (setf (sysop app) t)
			 (reset-menu app)
			 (setf (inner-html (main app)) "You are logged in."))
		       (setf (inner-html (main app)) "Invalid password."))))) 

(defun reset-menu (app)
  (setf (inner-html (side app)) "")
  (let ((menu-items (sqlite:execute-to-list *sql-connection*
					   "select menu from config")))
    (dolist (menu-item menu-items)
      (set-on-click 
       (create-web-sidebar-item (side app) :content (car menu-item))
       (lambda (obj)
	 (declare (ignore obj))
	 (route-content app (car menu-item))))))
  (create-br (side app))
  (if (sysop app)
      (progn
	(set-on-click (create-a (side app) :content "new")
		      (lambda (obj)
			(declare (ignore obj))
			(new-content app)))
	(create-br (side app))
	(set-on-click (create-a (side app) :content "logout")
		      (lambda (obj)
			(declare (ignore obj))
			(setf (sysop app) nil)
			(reset-menu app)
			(route-content app "Home"))))
      (set-on-click (create-a (side app) :content "login")
		    (lambda (obj)
		      (declare (ignore obj))
		      (id-me app)))))

(defun on-new-window (body)
  (set-html-on-close body "Connection Lost")
  ;; Create an app-data object for every connection
  (let ((app (make-instance 'app-data)))
    (setf (connection-data-item body "app-data") app)
    (clog-web-initialize body)
    (create-web-frame body app)
    (reset-menu app)
    (route-content app "Home")
    (run body)))

(defun start-demo ()
  "Start dynamic website demo."
  ;; The demo database is created in the clog dir
  (let ((db-dir (merge-pathnames "demo4.db" (clog:clog-install-dir))))
    (setf *sql-connection* (sqlite:connect db-dir))
    (format t "Database location: ~A~%" db-dir))
  (handler-case
      (setf *site-config*
	    (sqlite:execute-to-list *sql-connection* "select * from config"))
    (error ()
      (print "First run creating config.")
      (sqlite:execute-non-query
       *sql-connection*
       "create table config (menu varchar, main varchar)")
      (sqlite:execute-non-query
       *sql-connection*
       "insert into config (menu, main) values ('Home', '<b>Hello welcome.</b>')")
      (sqlite:execute-non-query
       *sql-connection*
       "insert into config (menu, main) values ('Page2', '<i>Customize Me.</i>')")))
  (initialize 'on-new-window)
  (open-browser))

(defun stop-demo ()
  "Shutdown demo and close databases."
  (sqlite:disconnect *sql-connection*)
  (shutdown))
