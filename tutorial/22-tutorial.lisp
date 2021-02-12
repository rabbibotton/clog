(defpackage #:clog-user
  (:use #:cl #:clog)
  (:export start-tutorial))

(in-package :clog-user)

(defun on-file-count (body)
  (let ((win (create-gui-window body)))
    (dotimes (n 100)
      (create-div (window-content win) :content n))))

(defun on-file-browse (body)
  (let* ((win     (create-gui-window body))
	 (browser (create-child (window-content win)
	    "<iframe width=100% height=98% src='https://common-lisp.net/'></iframe>")))))
  
(defun on-new-window (body)
  (clog-gui-initialize body)
  (add-class body "w3-cyan")  
  (let* ((menu  (create-gui-menu-bar body))
	 (icon  (create-gui-menu-icon menu))
	 (file  (create-gui-menu-drop-down menu :content "File"))
	 (new   (create-gui-menu-item file :content "Count" :on-click #'on-file-count))
	 (open  (create-gui-menu-item file :content "Browse" :on-click #'on-file-browse))
	 (save  (create-gui-menu-item file :content "Save"))
	 (help  (create-gui-menu-drop-down menu :content "Help"))
	 (about (create-gui-menu-item help :content "About"))
	 (fs    (create-gui-menu-full-screen menu))))

  (run body))

(defun start-tutorial ()
  "Start turtorial."

  (initialize #'on-new-window)
  (open-browser))
