(defpackage #:clog-tools
  (:use #:cl #:clog #:clog-gui)
  (:export clog-db-admin))

(in-package :clog-tools)

(defclass app-data ()
  ((body
    :accessor body
    :documentation "Top level access to browser window")
   (db-connection
    :accessor db-connection
    :initform nil
    :documentation "Access to database connection")))

(defun on-db-open (obj)
  (let* ((app (connection-data-item obj "app-data")))
    (form-dialog obj nil
		 '(("Database Type" :db-type (("SQLite3" :sqlite3)))
		   ("Database Name" :db-name))
		 (lambda (results)
		   (when results
		     (format t "open db : ~A" (cadr (assoc :db-name results)))
		     (setf (db-connection app)
			   (sqlite:connect (cadr (assoc :db-name results))))
		     (setf (title (html-document (body app)))
			   (format nil "CLOG DB Admin - ~A" (cadr (assoc :db-name results))))))
		 :title "Open Database" :height 250)))
		   
(defun on-db-close (obj)
  (let ((app (connection-data-item obj "app-data")))
    (when (db-connection app)
      (sqlite:disconnect (db-connection app))
      (setf (db-connection app) nil))
    (print "db disconnected")
    (setf (title (html-document (body app))) "CLOG DB Admin")))

(defun on-query-results (obj)
  (let ((app (connection-data-item obj "app-data")))
    (form-dialog obj nil
		 '(("Query" :db-query))
		 (lambda (results)
		   (when results
		    (format t "open query : ~A~%~%" (cadr (assoc :db-query results)))
		    (print (sqlite:execute-to-list
			    (db-connection app)
			    (cadr (assoc :db-query results))))))
		 :title "Run Database Query" :height 200)))
				
(defun on-help-about (obj)
  (let ((about (create-gui-window obj
				  :title   "About"
				  :content "<div class='w3-black'>
                                         <center><img src='/img/clogwicon.png'></center>
	                                 <center>CLOG</center>
	                                 <center>The Common Lisp Omnificent GUI</center></div>
			                 <div><p><center>CLOG DB Admin</center>
                                         <center>(c) 2021 - David Botton</center></p></div>"
				  :width   200
				  :height  200
				  :hidden  t)))
    (window-center about)
    (setf (visiblep about) t)    
    (set-on-window-can-size about (lambda (obj)
				    (declare (ignore obj))()))))

(defun on-new-window (body)
  (let ((app (make-instance 'app-data)))
    (setf (connection-data-item body "app-data") app)
    (setf (body app) body)
    (setf (title (html-document body)) "CLOG DB Admin")
    (clog-gui-initialize body)
    (add-class body "w3-blue-grey")  
    (let* ((menu  (create-gui-menu-bar body))
	   (tmp   (create-gui-menu-icon menu :on-click #'on-help-about))
	   (file  (create-gui-menu-drop-down menu :content "Database"))
	   (tmp   (create-gui-menu-item file :content "Open Connection" :on-click #'on-db-open))
	   (tmp   (create-gui-menu-item file :content "Close Connection" :on-click #'on-db-close))
	   (qry   (create-gui-menu-drop-down menu :content "Queries"))
	   (tmp   (create-gui-menu-item qry :content "Results Query" :on-click #'on-query-results))
	   (win   (create-gui-menu-drop-down menu :content "Window"))
	   (tmp   (create-gui-menu-item win :content "Maximize All" :on-click #'maximize-all-windows))
	   (tmp   (create-gui-menu-item win :content "Normalize All" :on-click #'normalize-all-windows))
	   (tmp   (create-gui-menu-window-select win))
	   (help  (create-gui-menu-drop-down menu :content "Help"))
	   (tmp   (create-gui-menu-item help :content "About" :on-click #'on-help-about))
	   (tmp   (create-gui-menu-full-screen menu))))
    (run body)
    (when (db-connection app)
      (sqlite:disconnect (db-connection app))
      (print "db disconnected"))))

(defun clog-db-admin ()
  "Start clog-db-admin."
  (initialize #'on-new-window)
  (open-browser))
