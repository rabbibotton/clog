;;; This is a template to help jump start a CLOG-GUI App

(defpackage #:clog-user
  (:use #:cl #:clog #:clog-gui)
  (:export start-tutorial))

(in-package :clog-user)

(defun on-file-new (obj)
  (let* ((win (create-gui-window obj :title "New Window")))
    ))

(defun on-help-about (obj)
  (let* ((about (create-gui-window obj
				   :title   "About"
				   :content "<div class='w3-black'>
                                         <center><img src='/img/clogwicon.png'></center>
	                                 <center>CLOG</center>
	                                 <center>The Common Lisp Omnificent GUI</center></div>
			                 <div><p><center>A New App</center>
                                         <center>(c) 2021 - David Botton</center></p></div>"
				   :hidden  t
				   :width   200
				   :height  200)))
    (window-center about)
    (setf (visiblep about) t)
    (set-on-window-can-size about (lambda (obj)
				    (declare (ignore obj))()))))

(defun on-new-window (body)
  (setf (title (html-document body)) "New App")
  (clog-gui-initialize body)
  (add-class body "w3-teal")  
  (let* ((menu  (create-gui-menu-bar body))
	 (tmp   (create-gui-menu-icon menu :on-click #'on-help-about))
	 (file  (create-gui-menu-drop-down menu :content "File"))
	 (tmp   (create-gui-menu-item file :content "New Window" :on-click #'on-file-new))
	 (help  (create-gui-menu-drop-down menu :content "Help"))
	 (tmp   (create-gui-menu-item help :content "About" :on-click #'on-help-about))
	 (tmp   (create-gui-menu-full-screen menu)))	 
  (run body)))

(defun start-app ()
  (initialize #'on-new-window)
  (open-browser))
