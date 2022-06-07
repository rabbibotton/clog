;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG New App - New CLOG App Templates                                 ;;;;
;;;; (c) 2020-2022 David Botton                                            ;;;;
;;;; License BSD 3 Clause                                                  ;;;;
;;;;                                                                       ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :clog-tools)

(defun on-new-app (body)
  "Launch instance of CLOG New Appp"
  (set-html-on-close body "Connection Lost")
  (let ((app (make-instance 'builder-app-data)))
    (setf (connection-data-item body "builder-app-data") app)
    (setf (title (html-document body)) "CLOG New App")
    (clog-gui-initialize body)
    (add-class body "w3-blue-grey")
    (let* ((menu  (create-gui-menu-bar body))
	   (icon  (create-gui-menu-icon menu :on-click #'on-help-about-builder))
	   (file  (create-gui-menu-drop-down menu :content "New App")))
      (declare (ignore icon))
      (create-gui-menu-item file  :content "New Application Template"  :on-click 'on-new-app-template)
      (create-gui-menu-full-screen menu))
    (set-on-before-unload (window body) (lambda(obj)
					  (declare (ignore obj))
					  ;; return empty string to prevent nav off page
					  ""))))
    
(defun clog-new-app (&key (port 8080) static-root)
  "Start clog-new-app."
  (if static-root
      (initialize nil :port port :static-root static-root)
      (initialize nil :port port))
  (set-on-new-window 'on-new-app :path "/new")
  (open-browser :url (format nil "http://127.0.0.1:~A/new" port)))
