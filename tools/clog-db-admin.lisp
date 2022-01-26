;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG Data - Database tool for CLOG                                    ;;;;
;;;; (c) 2020-2022 David Botton                                            ;;;;
;;;; License BSD 3 Clause                                                  ;;;;
;;;;                                                                       ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :clog-tools)

(defclass app-data ()
  ((body
    :accessor body
    :documentation "Top level access to browser window")
   (db-type
    :accessor db-type
    :initform nil
    :documentation "Database type")
   (indicator
    :accessor indicator
    :initform nil
    :documentation "Indicate connection")
   (db-connection
    :accessor db-connection
    :initform nil
    :documentation "Access to database connection")))

(defun on-db-open (obj)
  (let* ((app (connection-data-item obj "app-data")))
    (form-dialog obj nil
		 '(("Database Type" :db-type :select (("SQLite3" :sqlite3)))
		   ("Database Name" :db-name :filename "./"))
		 (lambda (results)
		   (when results
		     (setf (db-type app) (cadr (assoc :db-type results)))
		     (setf (db-connection app)
			   (sqlite:connect (cadr (assoc :db-name results))))
		     (remove-class (body app) "w3-blue-grey")
		     (add-class (body app) "w3-teal")
		     (setf (indicator app)
			   (create-child (body app)
					 "<div style='position:fixed;z-index:-9999;
                                                      bottom:0px;right:0px'><div>"))
		     (setf (inner-html (indicator app))
			   (cadr (assoc :db-name results)))
		     (setf (title (html-document (body app))) 
			   (format nil "CLOG DB Admin - ~A" (cadr (assoc :db-name results))))))
		 :title "Open Database" :height 250)))
		   
(defun on-db-close (obj)
  (let ((app (connection-data-item obj "app-data")))
    (when (db-connection app)
      (remove-class (body app) "w3-teal")
      (add-class (body app) "w3-blue-grey")
      (destroy (indicator app))
      (setf (indicator app) nil)
      (sqlite:disconnect (db-connection app))      
      (setf (db-connection app) nil))
    (setf (title (html-document (body app))) "CLOG DB Admin")))

(defun results-window (app sql &key (title nil) (on-click-row nil))
  (unless title
    (setf title sql))
  (let* ((prep  (sqlite:prepare-statement (db-connection app) sql))
	 (st    (sqlite:execute-to-list (db-connection app) sql))
	 (win   (create-gui-window (body app)
				   :width 500
				   :height 400
				   :title title))
	 (body  (window-content win))
	 (rt    (create-table body :class "w3-table-all w3-hover"))
	 (th    (create-table-head rt :class "w3-green"))
	 (names (sqlite:statement-column-names prep))
	 (cr))
    (dolist (name names)
      (create-table-heading th :content name))
    (dolist (row st)
      (setf cr (create-table-row rt))
      (when on-click-row
	(set-on-click cr (lambda (obj)
			   (funcall on-click-row obj names row))))
      (dolist (value row)
	(create-table-column cr :content value)))))

(defun on-query-results (obj)
  (let ((app (connection-data-item obj "app-data")))
    (when (db-connection app)
      (form-dialog obj nil
		   '(("Query" :db-query))
		   (lambda (results)
		     (when results
		       (handler-case
			   (results-window app (cadr (assoc :db-query results)))
			 (error (c)
			   (alert-dialog obj c :title "Error")))))
		   :title "Run Database Query" :height 200))))

(defun on-query-non (obj)
  (let ((app (connection-data-item obj "app-data")))
    (when (db-connection app)
      (form-dialog obj nil
		   '(("Non-Query" :db-query))
		   (lambda (results)
		     (when results
		       (format t "handle~%")
		       (handler-case
			   (progn
			     (sqlite:execute-non-query (db-connection app)
						       (cadr (assoc :db-query results)))
			     (results-window app "select changes()" :title (cadr (assoc :db-query results))))
			 (error (c)
			   (alert-dialog obj c :title "Error")))))
		   :title "Run Database Non-Query" :height 200))))

(defun edit-record (obj app table names data)
  (form-dialog obj "Edit Record"
	       (loop for x in names for z in data append (list (list x x :text z)))
	       (lambda (data)
		 (when data
		   (flet ((trim-last (s)
			    (subseq s 0 (- (length s) 1))))
		     (sqlite:execute-non-query
		      (db-connection app)
		      (format nil
			      "update ~A set ~A where rowid=~A"
			      table
			      (trim-last (format nil "~{~A~}"
						 (mapcar (lambda (l)
							   (if (equalp "rowid"
								       (first l))
							       ""
							       (format nil "~A='~A',"
								       (first l)
								       (second l))))
							 data)))
			      (cadar data))))
		   (results-window app "select changes()" :title table)))))

(defun on-query-tables (obj)
  (let ((app (connection-data-item obj "app-data")))
    (when (db-connection app)
      (results-window app "select tbl_name as 'Table', sql as SQL from sqlite_master where type='table'"
		      :title "Click for Table"
		      :on-click-row (lambda (obj names data)
				      (results-window app
						      (format nil "select rowid,* from ~A"
							      (car data))
						      :title (format nil "Click to Edit Row of ~A"
								     (car data))
						      :on-click-row
						      (lambda (obj names row)
							(edit-record obj app (car data) names row))))))))
				      
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
				  :height  215
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
	   (icon  (create-gui-menu-icon menu :on-click #'on-help-about))
	   (file  (create-gui-menu-drop-down menu :content "Database"))
	   (qry   (create-gui-menu-drop-down menu :content "Queries"))
	   (win   (create-gui-menu-drop-down menu :content "Window"))
	   (help  (create-gui-menu-drop-down menu :content "Help")))
      (declare (ignore icon))
      (create-gui-menu-item file :content "Open Connection" :on-click #'on-db-open)
      (create-gui-menu-item file :content "Close Connection" :on-click #'on-db-close)
      (create-gui-menu-item qry :content "Tables" :on-click #'on-query-tables)
      (create-gui-menu-item qry :content "Results Query" :on-click #'on-query-results)
      (create-gui-menu-item qry :content "Execute Non Query" :on-click #'on-query-non)
      (create-gui-menu-item win :content "Maximize All" :on-click #'maximize-all-windows)
      (create-gui-menu-item win :content "Normalize All" :on-click #'normalize-all-windows)
      (create-gui-menu-window-select win)
      (create-gui-menu-item help :content "About" :on-click #'on-help-about)
      (create-gui-menu-full-screen menu))
    (run body)
    (when (db-connection app)
      (sqlite:disconnect (db-connection app)))))

(defun clog-db-admin ()
  "Start clog-db-admin."
  (initialize #'on-new-window)
  (open-browser))
