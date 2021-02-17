(defpackage #:clog-tools
  (:use #:cl #:clog)
  (:export clog-db-admin))

(in-package :clog-tools)

(defclass app-data ()
  ((body
    :accessor body
    :documentation "Top level access to browser window")))

(defun on-help-about (obj)
  (let* ((app (connection-data-item obj "app-data"))
	 (about (create-gui-window obj
				   :title   "About"
				   :content "<div class='w3-black'>
                                         <center><img src='/img/clogwicon.png'></center>
	                                 <center>CLOG</center>
	                                 <center>The Common Lisp Omnificent GUI</center></div>
			                 <div><p><center>CLOG DB Admin</center>
                                         <center>(c) 2021 - David Botton</center></p></div>"
				   :left (- (/ (inner-width (window (body app))) 2.0) 100)
				   :top (- (/ (inner-height (window (body app))) 2.0) 100)
				   :width   200
				   :height  200)))
    (print (- (/ (inner-width (window (body app))) 2.0) 100))
    (set-on-window-can-size about (lambda (obj)
				    (declare (ignore obj))()))))

(defun on-new-window (body)
  (let ((app (make-instance 'app-data)))
    (setf (connection-data-item body "app-data") app)
    (setf (body app) body))
  (setf (title (html-document body)) "CLOG DB Admin")
  (clog-gui-initialize body)
  (add-class body "w3-blue-grey")  
  (let* ((menu  (create-gui-menu-bar body))
	 (tmp   (create-gui-menu-icon menu :on-click #'on-help-about))
	 (file  (create-gui-menu-drop-down menu :content "File"))
	 (tmp   (create-gui-menu-drop-down menu :content "Open Database Connection"))
	 (win   (create-gui-menu-drop-down menu :content "Window"))
	 (tmp   (create-gui-menu-item win :content "Maximize All" :on-click #'maximize-all-windows))
	 (tmp   (create-gui-menu-item win :content "Normalize All" :on-click #'normalize-all-windows))
	 (tmp   (create-gui-menu-window-select win))
	 (help  (create-gui-menu-drop-down menu :content "Help"))
	 (tmp   (create-gui-menu-item help :content "About" :on-click #'on-help-about))
	 (tmp   (create-gui-menu-full-screen menu))))
  (run body))

(defun clog-db-admin ()
  "Start clog-db-admin."
  (initialize #'on-new-window)
  (open-browser))
