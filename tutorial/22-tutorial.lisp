(defpackage #:clog-user
  (:use #:cl #:clog)
  (:export start-tutorial))

(in-package :clog-user)

(defun on-new-window (body)
  (clog-gui-initialize body)
  (add-class body "w3-teal")  
  (let* ((menu  (create-gui-menu-bar body))
	 (icon  (create-gui-menu-icon menu))
	 (file  (create-gui-menu-drop-down menu :content "File"))
	 (new   (create-gui-menu-item file :content "New"))
	 (open  (create-gui-menu-item file :content "Open"))
	 (save  (create-gui-menu-item file :content "Save"))
	 (help  (create-gui-menu-drop-down menu :content "Help"))
	 (about (create-gui-menu-item help :content "About"))
	 (fs    (create-gui-menu-full-screen menu))))
  
  (run body))

(defun start-tutorial ()
  "Start turtorial."

  (initialize #'on-new-window)
  (open-browser))
