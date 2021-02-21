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
  (let* ((app (connection-data-item obj "app-data"))
	 (win (create-gui-window obj
				 :title "Open Database"
				 :content
"<form id=odb-form class='w3-container' onSubmit='return false;'>
<label class='w3-text-black'><b>Database Type</b></label>
<select class='w3-select w3-border' name='db-type'>
<option value='sqlite3'>SQLite3</option>
<option disabled value='mysql'>MySQL</option>
<option disabled value='postgres'>Postgres</option>
</select>

<label class='w3-text-black'><b>Database Name</b></label>
<input class='w3-input w3-border' type='text' name='db-name'>
 
<label class='w3-text-black'><b>User Name</b></label>
<input class='w3-input w3-border' type='text' name='db-user' disabled>

<label class='w3-text-black'><b>Password</b></label>
<input class='w3-input w3-border' type='password' name='db-pass' disabled>

<label class='w3-text-black'><b>Host</b></label>
<input class='w3-input w3-border' type='password' name='db-host' disabled>

<label class='w3-text-black'><b>Port</b></label>
<input class='w3-input w3-border' type='password' name='db-port' disabled>

<button class='w3-btn w3-black' id=odb-open >Open</button>
<button class='w3-btn w3-black' id=odb-cancel>Cancel</button>
 
</form>"
				 :width  400
				 :height 450
				 :hidden t)))
    (window-center win)
    (setf (visiblep win) t)
    (set-on-click (attach-as-child obj "odb-open")
		  (lambda (obj)
		    (format t "open db : ~A" (name-value obj "db-name"))
		    (setf (db-connection app)
			  (sqlite:connect (name-value obj "db-name")))
		    (setf (title (html-document (body app)))
			  (format nil "CLOG DB Admin - ~A" (name-value obj "db-name")))
		    (window-close win))
		  :one-time t)

    (set-on-click (attach-as-child obj "odb-cancel") (lambda (obj)
						       (window-close win)))))
(defun on-db-close (obj)
  (let ((app (connection-data-item obj "app-data")))
    (when (db-connection app)
      (sqlite:disconnect (db-connection app)))
    (print "db disconnected")
    (setf (title (html-document (body app))) "CLOG DB Admin")))

(defun on-query-results (obj)
  (let ((app (connection-data-item obj "app-data"))
	(win (create-gui-window obj
				:title "Enter query:"
				:content
"<form id=odb-form class='w3-container' onSubmit='return false;'>

<label class='w3-text-black'><b>Query</b></label>
<input class='w3-input w3-border' type='text' name='db-query'>
<button class='w3-btn w3-black' id=odb-open >Open</button>
<button class='w3-btn w3-black' id=odb-cancel>Cancel</button>
 
</form>"
				:width  400
				:height 200
				:hidden t)))
    (window-center win)
    (setf (visiblep win) t)
    (set-on-click (attach-as-child obj "odb-open")
		  (lambda (obj)
		    (format t "open query : ~A~%~%" (name-value obj "db-query"))
		    (print (sqlite:execute-to-list
			    (db-connection app)
			    (name-value obj "db-query")))
		    (window-close win)))

    (set-on-click (attach-as-child obj "odb-cancel") (lambda (obj)
						       (window-close win)))))
				
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
